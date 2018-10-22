import
  future, options, os, posix, re, sequtils, strutils,
  args, config, format, pacman, utils

import
  "feature/localquery",
  "feature/syncclean",
  "feature/syncinfo",
  "feature/syncinstall",
  "feature/syncsearch",
  "feature/syncsource"

proc execSudo*(args: seq[Argument]): int =
  execResult(sudoPrefix & getAppFilename() &
    lc[x | (y <- args, x <- y.collectArg), string])

proc passValidation(args: seq[Argument], config: Config,
  nonRootArgs: openArray[OptionPair], rootArgs: openArray[OptionPair],
  opts: varargs[seq[CommandOption]]): int =
  let checkArgs = args.filterOptions(true, false, true, opts)

  if checkArgs.len == 0:
    let needRoot = (nonRootArgs.len == 0 and args.check(rootArgs)) or
      (nonRootArgs.len > 0 and (not args.check(nonRootArgs) or args.check(rootArgs)))
    return pacmanExec(needRoot, config.color, args.filterExtensions(true, true, opts))
  else:
    let extensions = args.filterExtensions(false, false, opts)
    if extensions.len == 0:
      return pacmanExec(false, config.color, args)
    else:
      let arg = extensions[0]
      if arg.isShort:
        raise commandError(trp("invalid option '-%c'\n").strip
          .replace("%c", "$#") % arg.key)
      else:
        raise commandError(trp("invalid option '--%s'\n").strip
          .replace("%s", "$#") % arg.key)

proc handleDatabase(args: seq[Argument], config: Config): int =
  let nonRootArgs = [
    %%%"check"
  ]

  passValidation(args, config, nonRootArgs, [],
    commonOptions, databaseOptions)

proc handleFiles(args: seq[Argument], config: Config): int =
  let rootArgs = [
    %%%"refresh"
  ]

  passValidation(args, config, [], rootArgs,
    commonOptions, filesOptions)

proc handleQuery(args: seq[Argument], config: Config): int =
  let queryArgs = args.removeMatchOptions(commonOptions)

  if queryArgs.checkOpGroup(OpGroup.localQuery) and
    not queryArgs.check(%%%"explicit") and
    queryArgs.check(%%%"deps") and
    queryArgs.count(%%%"unrequired") >= 3:
    handleQueryOrphans(args, config)
  else:
    passValidation(args, config, [], [],
      commonOptions, queryOptions)

proc handleRemove(args: seq[Argument], config: Config): int =
  let nonRootArgs = [
    %%%"print",
    %%%"print-format"
  ]

  passValidation(args, config, nonRootArgs, [],
    commonOptions, transactionOptions, removeOptions)

proc handleSync(args: seq[Argument], config: Config): int =
  let syncArgs = args.removeMatchOptions(commonOptions, transactionOptions, upgradeOptions)
  let conflict = args.checkConflicts(syncConflictingOptions)

  if conflict.isSome:
    let (left, right) = conflict.unsafeGet
    printError(config.color, trp("invalid option: '%s' and '%s' may not be used together\n") %
      ["--" & left, "--" & right])
    1
  elif syncArgs.check(%%%"clean"):
    handleSyncClean(args, config)
  elif syncArgs.check(%%%"info") and
    syncArgs.checkOpGroup(OpGroup.syncQuery):
    handleSyncInfo(args, config)
  elif syncArgs.check(%%%"search") and
    syncArgs.checkOpGroup(OpGroup.syncSearch):
    handleSyncSearch(args, config)
  elif syncArgs.check(%%%"source"):
    handleSyncSource(args, config)
  elif syncArgs.checkOpGroup(OpGroup.syncInstall) and
    (args.check(%%%"sysupgrade") or args.targets.len > 0):
    let printMode = args.check(%%%"print") or args.check(%%%"print-format")

    if currentUser.uid != 0 and config.sudoExec and not printMode:
      execSudo(args)
    else:
      let isNonDefaultRoot = not config.common.defaultRoot
      let isRootNoDrop = currentUser.uid == 0 and not canDropPrivileges()

      let build = args.check(%%%"build")
      let noaur = args.check(%%%"noaur")

      let noBuild = isNonDefaultRoot or isRootNoDrop

      if not printMode and build and noBuild:
        if isNonDefaultRoot:
          printError(config.color, tr"non-default root path is specified" & " -- " &
            tr"building is not allowed")
        elif isRootNoDrop:
          printError(config.color, tr"running as root" & " -- " &
            tr"building is not allowed")
        1
      else:
        let noaurAdd = not printMode and noBuild and not noaur

        if noaurAdd:
          if isNonDefaultRoot:
            printWarning(config.color, tr"non-default root path is specified" & " -- " &
              tr"'$#' is assumed" % ["--noaur"])
          elif isRootNoDrop:
            printWarning(config.color, tr"running as root" & " -- " &
              tr"'$#' is assumed" % ["--noaur"])

        if noaurAdd:
          handleSyncInstall(args & ("noaur", none(string), ArgumentType.long), config)
        else:
          handleSyncInstall(args, config)
  else:
    let nonRootArgs = [
      %%%"print",
      %%%"print-format",
      %%%"groups",
      %%%"info",
      %%%"list",
      %%%"search"
    ]

    let rootArgs = [
      %%%"refresh"
    ]

    passValidation(args, config, nonRootArgs, rootArgs,
      commonOptions, transactionOptions, upgradeOptions, syncOptions)

proc handleDeptest(args: seq[Argument], config: Config): int =
  passValidation(args, config, [], [], commonOptions)

proc handleUpgrade(args: seq[Argument], config: Config): int =
  let nonRootArgs = [
    %%%"print",
    %%%"print-format"
  ]

  passValidation(args, config, nonRootArgs, [],
    commonOptions, transactionOptions, upgradeOptions)

proc handleHelp(operation: OperationType) =
  proc printHelp(pair: OptionPair, arg: Option[string], text: string) =
    let shortPrefix = pair.short.map(s => "  -" & s & ", ").get(' '.repeat(6))
    let longValue = arg.map(a => pair.long & " <" & a & ">").get(pair.long)

    if longValue.len > 14:
      echo(shortPrefix, "--", longValue)
      echo(' '.repeat(23), text)
    else:
      echo(shortPrefix, "--", longValue, ' '.repeat(15 - longValue.len), text)

  let operationArgs = operations
    .filter(o => o.otype == operation)
    .map(o => @["-" & o.pair.short.get])
    .optFirst.get(@[]) & @["-h"]

  let (lines, _) = forkWaitRedirect(() => execResult(pacmanCmd & operationArgs))

  for line in lines:
    echo(line.replace(re"\bpacman\b", "pakku"))

  if lines.len > 0:
    case operation:
      of OperationType.sync:
        printHelp(%%%"build", none(string), tr"build targets from source")
        printHelp(%%%"keyserver", some("name"), tr"use name as keyserver to receive keys from")
        printHelp(%%%"noaur", none(string), tr"disable all AUR operations")
        printHelp(%%%"source", none(string), tr"retrieve PKGBUILD source")
      else:
        discard

const
  version = getEnv("PROG_VERSION")
  copyright = getEnv("PROG_COPYRIGHT")

proc handleVersion(): int =
  echo()
  echo(' '.repeat(23), "Pakku v", version)
  echo(' '.repeat(23), "Copyright (C) ", copyright)
  pacmanExec(false, false, ("V", none(string), ArgumentType.short))

discard setlocale(LC_ALL, "")

template withErrorHandler(propColor: Option[bool], T: typedesc, body: untyped):
  tuple[success: Option[T], code: int] =
  try:
    (some(body), 0)
  except HaltError:
    let e = (ref HaltError) getCurrentException()
    (none(T), e.code)
  except CommandError:
    let e = (ref CommandError) getCurrentException()
    if e.error:
      printError(e.color.orElse(propColor).get(false), e.msg)
    else:
      stderr.writeLine(e.msg)
    (none(T), 1)

let init = withErrorHandler(none(bool),
  tuple[parsedArgs: seq[Argument], config: Config]):
  let parsedArgs = splitArgs(commandLineParams(), optionsWithParameter,
    (operations.map(o => o.pair.long) & allOptions.map(o => o.pair.long)).deduplicate)
  let operation = getOperation(parsedArgs)
  if operation != OperationType.invalid and
    parsedArgs.check(%%%"help"):
    handleHelp(operation)
    raise haltError(0)
  elif operation != OperationType.invalid and
    parsedArgs.check((some("V"), "version")):
    let code = handleVersion()
    raise haltError(code)
  elif parsedArgs.check(%%%"sysroot") and currentUser.uid != 0:
    let code = execSudo(parsedArgs)
    raise haltError(code)
  else:
    let pacmanConfig = obtainPacmanConfig(parsedArgs)
    let config = obtainConfig(pacmanConfig)
    (parsedArgs, config)

proc run(parsedArgs: seq[Argument], config: Config):
  tuple[success: Option[int], code: int] =
  withErrorHandler(some(config.color), int):
    case getOperation(parsedArgs):
      of OperationType.database:
        handleDatabase(parsedArgs, config)
      of OperationType.files:
        handleFiles(parsedArgs, config)
      of OperationType.query:
        handleQuery(parsedArgs, config)
      of OperationType.remove:
        handleRemove(parsedArgs, config)
      of OperationType.sync:
        handleSync(parsedArgs, config)
      of OperationType.deptest:
        handleDeptest(parsedArgs, config)
      of OperationType.upgrade:
        handleUpgrade(parsedArgs, config)
      else:
        passValidation(parsedArgs, config, [], [], allOptions)

let runResult = if init.success.isSome:
    run(init.success.unsafeGet.parsedArgs, init.success.unsafeGet.config)
  else:
    (none(int), init.code)

programResult = if runResult.success.isSome:
    runResult.success.unsafeGet
  else:
    runResult.code

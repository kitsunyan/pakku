import
  future, options, os, posix, re, sequtils, strutils,
  args, config, format, pacman, utils

import
  "feature/syncinfo",
  "feature/syncsearch",
  "feature/syncinstall",
  "feature/localquery"

proc passValidation(args: seq[Argument], config: Config,
  nonRootArgs: openArray[OptionPair], rootArgs: openArray[OptionPair],
  opts: varargs[seq[CommandOption]]): int =
  let checkArgs = args.filterOptions(true, false, true, opts)

  if checkArgs.len == 0:
    let needRoot = (nonRootArgs.len == 0 and args.check(rootArgs)) or
      (nonRootArgs.len > 0 and (not args.check(nonRootArgs) or args.check(rootArgs)))
    return pacmanExec(needRoot, config.color, args.filterExtensions(true, true))
  else:
    let extensions = args.filterExtensions(false, false)
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
    (some("k"), "check")
  ]

  passValidation(args, config, nonRootArgs, [],
    commonOptions, databaseOptions)

proc handleFiles(args: seq[Argument], config: Config): int =
  let rootArgs = [
    (some("y"), "refresh")
  ]

  passValidation(args, config, [], rootArgs,
    commonOptions, filesOptions)

proc handleQuery(args: seq[Argument], config: Config): int =
  let queryArgs = args.removeMatchOptions(commonOptions)

  if queryArgs.checkOpGroup(OpGroup.localQuery) and
    not queryArgs.check((some("e"), "explicit")) and
    queryArgs.check((some("d"), "deps")) and
    queryArgs.count((some("t"), "unrequired")) >= 3:
    handleQueryOrphans(args, config)
  else:
    passValidation(args, config, [], [],
      commonOptions, queryOptions)

proc handleRemove(args: seq[Argument], config: Config): int =
  let nonRootArgs = [
    (some("p"), "print"),
    (none(string), "print-format")
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
  elif syncArgs.check((some("i"), "info")) and
    syncArgs.checkOpGroup(OpGroup.syncQuery):
    handleSyncInfo(args, config)
  elif syncArgs.check((some("s"), "search")) and
    syncArgs.checkOpGroup(OpGroup.syncSearch):
    handleSyncSearch(args, config)
  elif syncArgs.checkOpGroup(OpGroup.syncInstall) and
    (args.check((some("u"), "sysupgrade")) or args.targets.len > 0):
    if currentUser.uid != 0 and config.sudoExec:
      let collectedArgs = @[sudoCmd, getAppFilename()] &
        lc[x | (y <- args, x <- y.collectArg), string]
      execResult(collectedArgs)
    else:
      let isNonDefaultRoot = not config.isRootDefault
      let isDowngrade = args.count((some("u"), "sysupgrade")) >= 2
      let isSkipDeps = args.check((some("d"), "nodeps"))
      let isRootNoDrop = currentUser.uid == 0 and not canDropPrivileges()

      let build = args.check((none(string), "build"))
      let noaur = args.check((none(string), "noaur"))

      let noBuild = isNonDefaultRoot or isDowngrade or isSkipDeps or isRootNoDrop

      if build and noBuild:
        if isNonDefaultRoot:
          printError(config.color, tr"non-default root path is specified" & " -- " &
            tr"building is not allowed")
        elif isDowngrade:
          printError(config.color, tr"downgrades are enabled" & " -- " &
            tr"building is not allowed")
        elif isSkipDeps:
          printError(config.color, tr"dependency check is skipped" & " -- " &
            tr"building is not allowed")
        elif isRootNoDrop:
          printError(config.color, tr"running as root" & " -- " &
            tr"building is not allowed")
        1
      else:
        let noaurAdd = noBuild and not noaur

        if noaurAdd:
          if isNonDefaultRoot:
            printWarning(config.color, tr"non-default root path is specified" & " -- " &
              tr"'$#' is assumed" % ["--noaur"])
          elif isDowngrade:
            printWarning(config.color, tr"downgrades are enabled" & " -- " &
              tr"'$#' is assumed" % ["--noaur"])
          elif isSkipDeps:
            printWarning(config.color, tr"dependency check is skipped" & " -- " &
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
      (some("p"), "print"),
      (none(string), "print-format"),
      (some("g"), "groups"),
      (some("i"), "info"),
      (some("l"), "list"),
      (some("s"), "search")
    ]

    let rootArgs = [
      (some("y"), "refresh"),
    ]

    passValidation(args, config, nonRootArgs, rootArgs,
      commonOptions, transactionOptions, upgradeOptions, syncOptions)

proc handleDeptest(args: seq[Argument], config: Config): int =
  passValidation(args, config, [], [], commonOptions)

proc handleUpgrade(args: seq[Argument], config: Config): int =
  let nonRootArgs = [
    (some("p"), "print"),
    (none(string), "print-format")
  ]

  passValidation(args, config, nonRootArgs, [],
    commonOptions, transactionOptions, upgradeOptions)

proc handleHelp(operation: OperationType) =
  proc printHelp(command: string, text: string) =
    if command.len > 14:
      echo(' '.repeat(6), "--", command)
      echo(' '.repeat(23), text)
    else:
      echo(' '.repeat(6), "--", command, ' '.repeat(15 - command.len), text)

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
        printHelp("build", tr"build targets from source")
        printHelp("keyserver <name>", tr"use name as keyserver to receive keys from")
        printHelp("noaur", tr"disable all AUR operations")
      else:
        discard

const
  version = $getenv("PROG_VERSION")
  copyright = $getenv("PROG_COPYRIGHT")

proc handleVersion(): int =
  echo()
  echo(' '.repeat(23), "Pakku v", version)
  echo(' '.repeat(23), "Copyright (C) ", copyright)
  pacmanExec(false, false, ("V", none(string), ArgumentType.short))

proc signal(sign: cint, handler: pointer): pointer
  {.importc, header: "<signal.h>".}

discard setlocale(LC_ALL, "")
discard signal(SIGINT, cast[pointer](SIG_DFL))

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
  let parsedArgs = splitArgs(commandLineParams(), optionsWithParameter)
  let pacmanConfig = obtainPacmanConfig(parsedArgs)
  let config = obtainConfig(pacmanConfig)
  (parsedArgs, config)

proc run(parsedArgs: seq[Argument], config: Config):
  tuple[success: Option[int], code: int] =
  withErrorHandler(some(config.color), int):
    let operation = getOperation(parsedArgs)
    if operation != OperationType.invalid and
      parsedArgs.check((some("h"), "help")):
      handleHelp(operation)
      0
    elif operation != OperationType.invalid and
      parsedArgs.check((some("V"), "version")):
      handleVersion()
    else:
      case operation:
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
          pacmanExec(false, config.color,
            parsedArgs.filterExtensions(true, true))

let runResult = if init.success.isSome:
    run(init.success.unsafeGet.parsedArgs, init.success.unsafeGet.config)
  else:
    (none(int), init.code)

programResult = if runResult.success.isSome:
    runResult.success.unsafeGet
  else:
    runResult.code

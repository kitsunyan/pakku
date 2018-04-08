import
  future, macros, options, os, posix, re, sequtils, sets, strutils, tables,
  args, config, utils

type
  OpGroup* {.pure.} = enum
    syncInstall, syncSearch, syncQuery, localQuery

  OperationType* {.pure.} = enum
    unknown, invalid, database, files, query,
    remove, sync, deptest, upgrade

  Operation* = tuple[
    pair: OptionPair,
    otype: OperationType
  ]

  CommandOption* = tuple[
    pair: OptionPair,
    hasParam: bool,
    extension: bool,
    groups: set[OpGroup]
  ]

  ConflictingOptions* = tuple[
    left: string,
    right: seq[string]
  ]

proc calculateOptionsWithParameter(opts: seq[CommandOption]): seq[OptionKey] {.compileTime.} =
  proc commandToSeq(co: CommandOption): seq[OptionKey] {.compileTime.} =
    if co.hasParam:
      co.pair.short
        .map(s => @[(s, false), (co.pair.long, true)])
        .get(@[(co.pair.long, true)])
    else:
      @[]

  lc[x | (y <- opts, x <- commandToSeq(y)), OptionKey]

proc o(long: string): CommandOption {.compileTime.} =
  ((none(string), long), false, false, {})

proc o(short: string, long: string): CommandOption {.compileTime.} =
  ((some(short), long), false, false, {})

proc `^`(opt: CommandOption): CommandOption {.compileTime.} =
  (opt.pair, not opt.hasParam, opt.extension, opt.groups)

proc `$`(opt: CommandOption): CommandOption {.compileTime.} =
  (opt.pair, opt.hasParam, not opt.extension, opt.groups)

proc `+`(opt: CommandOption, groups: set[OpGroup]): CommandOption {.compileTime.} =
  (opt.pair, opt.hasParam, opt.extension, opt.groups + groups)

macro g(gls: varargs[untyped]): untyped =
  result = newNimNode(nnkCurly, gls)
  for gl in gls:
    add(result, newDotExpr(ident("OpGroup"), gl))

const
  operations*: seq[Operation] = @[
    ((some("D"), "database"), OperationType.database),
    ((some("F"), "files"), OperationType.files),
    ((some("Q"), "query"), OperationType.query),
    ((some("R"), "remove"), OperationType.remove),
    ((some("S"), "sync"), OperationType.sync),
    ((some("T"), "deptest"), OperationType.deptest),
    ((some("U"), "upgrade"), OperationType.upgrade)
  ]

  commonOptions*: seq[CommandOption] = @[
    ^o("b", "dbpath"),
    ^o("r", "root"),
    o("v", "verbose"),
    ^o("arch"),
    ^o("cachedir"),
    ^o("color"),
    ^o("config"),
    o("debug"),
    ^o("gpgdir"),
    ^o("hookdir"),
    ^o("logfile"),
    o("noconfirm"),
    o("confirm")
  ]

  transactionOptions*: seq[CommandOption] = @[
    o("d", "nodeps"),
    ^o("assume-installed"),
    o("dbonly"),
    o("noprogressbar"),
    o("noscriptlet"),
    o("p", "print"),
    ^o("print-format")
  ]

  upgradeOptions*: seq[CommandOption] = @[
    o("force"),
    o("asdeps"),
    o("asexplicit"),
    ^o("ignore"),
    ^o("ignoregroup"),
    o("needed")
  ]

  queryOptions*: seq[CommandOption] = @[
    o("c", "changelog") + g(localQuery),
    o("d", "deps") + g(localQuery),
    o("e", "explicit") + g(localQuery),
    o("g", "groups"),
    o("i", "info") + g(localQuery),
    o("k", "check") + g(localQuery),
    o("l", "list") + g(localQuery),
    o("m", "foreign") + g(localQuery),
    o("n", "native") + g(localQuery),
    o("o", "owns"),
    o("p", "file"),
    o("q", "quiet") + g(localQuery),
    o("s", "search"),
    o("t", "unrequired") + g(localQuery),
    o("u", "upgrades") + g(localQuery)
  ]

  removeOptions*: seq[CommandOption] = @[
    o("c", "cascade"),
    o("n", "nosave"),
    o("s", "recursive"),
    o("u", "unneeded")
  ]

  syncOptions*: seq[CommandOption] = @[
    o("c", "clean"),
    o("g", "groups"),
    o("i", "info") + g(syncInstall, syncQuery),
    o("l", "list"),
    o("q", "quiet") + g(syncInstall, syncSearch, syncQuery),
    o("s", "search") + g(syncSearch),
    o("u", "sysupgrade") + g(syncInstall),
    o("w", "downloadonly"),
    o("y", "refresh") + g(syncInstall, syncSearch, syncQuery),
    $o("build") + g(syncInstall),
    $(^o("keyserver")) + g(syncInstall),
    $o("noaur") + g(syncInstall)
  ]

  databaseOptions*: seq[CommandOption] = @[
    o("asdeps"),
    o("asexplicit"),
    o("k", "check")
  ]

  filesOptions*: seq[CommandOption] = @[
    o("y", "refresh"),
    o("l", "list"),
    o("s", "search"),
    o("x", "regex"),
    o("o", "owns"),
    o("q", "quiet"),
    o("machinereadable")
  ]

  upgradeCommonOptions*: seq[CommandOption] = @[
    o("noprogressbar"),
    o("force")
  ]

  allOptions = commonOptions & transactionOptions &
    upgradeOptions & queryOptions & removeOptions & syncOptions &
    databaseOptions & filesOptions

  optionsWithParameter*: HashSet[OptionKey] =
    calculateOptionsWithParameter(allOptions).toSet

  syncConflictingOptions*: seq[ConflictingOptions] = @[
    ("asdeps", @["asexplicit"]),
    ("build", @["nodeps", "assume-installed", "dbonly", "clean",
      "groups", "info", "list", "search", "sysupgrade", "downloadonly"]),
    ("keyserver", @["clean", "groups", "info", "list", "search"])
  ]

  allConflictingOptions = syncConflictingOptions

proc checkOptions(check: seq[CommandOption],
  where: openArray[seq[CommandOption]]) {.compileTime.} =
  let whereSeq = @where
  let whereSet = lc[x.pair | (y <- whereSeq, x <- y), OptionPair].toSet
  for c in check:
    if not (c.pair in whereSet):
      raise newException(SystemError,
        "invalid options definition: " & $c.pair)

static:
  # options test
  checkOptions(upgradeCommonOptions, [commonOptions, transactionOptions, upgradeOptions])

proc getOperation*(args: seq[Argument]): OperationType =
  let matchedOps = args
    .map(arg => operations
      .filter(o => (arg.isShort and some(arg.key) == o.pair.short) or
        (arg.isLong and arg.key == o.pair.long)))
    .filter(ops => ops.len > 0)

  if matchedOps.len == 0:
    OperationType.unknown
  elif matchedOps.len == 1:
    matchedOps[0][0].otype
  else:
    OperationType.invalid

proc filterOptions*(args: seq[Argument], removeMatches: bool, keepTargets: bool,
  includeOptions: bool, opts: varargs[seq[CommandOption]]): seq[Argument] =
  let optsSeq = @opts
  let optsPairSeq = lc[x.pair | (y <- optsSeq, x <- y), OptionPair]

  let work = if includeOptions:
      (optsPairSeq & operations.map(o => o.pair))
    else:
      optsPairSeq

  args.filter(removeMatches, keepTargets, work)

template removeMatchOptions*(args: seq[Argument],
  opts: varargs[seq[CommandOption]]): seq[Argument] =
  filterOptions(args, true, true, true, opts)

template keepOnlyOptions*(args: seq[Argument],
  opts: varargs[seq[CommandOption]]): seq[Argument] =
  filterOptions(args, false, false, false, opts)

proc checkValid*(args: seq[Argument], opts: varargs[seq[CommandOption]]): bool =
  filterOptions(args, true, false, true, opts).len == 0

proc checkOpGroup*(args: seq[Argument], group: OpGroup): bool =
  let toCheck = allOptions
    .filter(o => group in o.groups)
    .map(o => o.pair)

  args.whitelisted(toCheck)

proc filterExtensions*(args: seq[Argument],
  removeMatches: bool, keepTargets: bool): seq[Argument] =
  let argsSeq = lc[x.pair | (x <- allOptions, x.extension), OptionPair]
  args.filter(removeMatches, keepTargets, argsSeq)

proc obtainConflictsPairs(conflicts: seq[ConflictingOptions]): Table[string, seq[OptionPair]] =
  let all = lc[x | (y <- conflicts, x <- y.left & y.right), string].deduplicate
  all.map(c => (c, allOptions.filter(o => o.pair.long == c)
    .map(o => o.pair).deduplicate)).toTable

static:
  # conflicting options test
  for name, pairs in allConflictingOptions.obtainConflictsPairs:
    if pairs.len != 1:
      raise newException(SystemError,
        "invalid conflicts definition: " & name & " " & $pairs)

proc checkConflicts*(args: seq[Argument],
  conflicts: seq[ConflictingOptions]): Option[(string, string)] =
  let table = conflicts.obtainConflictsPairs
  template full(s: string): OptionPair = table[s][0]

  lc[(c.left, w) | (c <- conflicts, args.check(c.left.full),
    w <- c.right, args.check(w.full)), (string, string)].optFirst

proc checkExec(file: string): bool =
  var statv: Stat
  stat(file, statv) == 0 and (statv.st_mode and S_IXUSR) == S_IXUSR

proc pacmanExec(root: bool, args: varargs[string]): int =
  let exec = if root and checkExec(sudoCmd):
      @[sudoCmd, pacmanCmd] & @args
    elif root and checkExec(suCmd):
      @[suCmd, "root", "-c", "exec \"$@\"", "--", "sh", pacmanCmd] & @args
    else:
      @[pacmanCmd] & @args

  execResult(exec)

proc pacmanExec*(root: bool, color: bool, args: varargs[Argument]): int =
  let useRoot = root and getuid() != 0
  let colorStr = if color: "always" else: "never"

  let argsSeq = ("color", some(colorStr), ArgumentType.long) &
    @args.filter(arg => not arg.matchOption((none(string), "color")))
  let collectedArgs = lc[x | (y <- argsSeq, x <- y.collectArg), string]

  pacmanExec(useRoot, collectedArgs)

proc pacmanRun*(root: bool, color: bool, args: varargs[Argument]): int =
  let argsSeq = @args
  forkWait(() => pacmanExec(root, color, argsSeq))

proc pacmanValidateAndThrow(args: varargs[Argument]): void =
  let argsSeq = @args
  let collectedArgs = lc[x | (y <- argsSeq, x <- y.collectArg), string]
  let code = forkWait(() => pacmanExec(false, "-T" & collectedArgs))
  if code != 0:
    raise haltError(code)

proc getMachineName: Option[string] =
  var utsname: Utsname
  let length = if uname(utsname) == 0: utsname.machine.find('\0') else: -1
  if length > 0: some(utsname.machine.toString(some(length))) else: none(string)

proc createConfigFromTable(table: Table[string, string], dbs: seq[string]): PacmanConfig =
  let root = table.opt("RootDir")
  let db = table.opt("DBPath")
  let gpg = table.opt("GPGDir")
  let color = if table.hasKey("Color"): ColorMode.colorAuto else: ColorMode.colorNever
  let verbosePkgList = table.hasKey("VerbosePkgLists")
  let arch = table.opt("Architecture").get("auto")
  let ignorePkgs = table.opt("IgnorePkg").get("").splitWhitespace.toSet
  let ignoreGroups = table.opt("IgnoreGroup").get("").splitWhitespace.toSet

  let archFinal = if arch.len == 0 or arch == "auto": getMachineName().get(arch) else: arch
  if archFinal.len == 0 or archFinal == "auto":
    raise commandError(tr"can not get the architecture",
      colorNeeded = some(color.get))

  PacmanConfig(rootOption: root, dbOption: db, gpgOption: gpg, dbs: dbs, arch: archFinal,
    colorMode: color, debug: false, progressBar: true, verbosePkgList: verbosePkgList,
    pgpKeyserver: none(string), ignorePkgs: ignorePkgs, ignoreGroups: ignoreGroups)

proc obtainPacmanConfig*(args: seq[Argument]): PacmanConfig =
  proc getAll(pair: OptionPair): seq[string] =
    args.filter(arg => arg.matchOption(pair)).map(arg => arg.value.get)

  let configFile = getAll((none(string), "config")).optLast.get(sysConfDir & "/pacman.conf")
  let (configTable, wasError) = readConfigFile(configFile)

  let options = configTable.opt("options").map(t => t[]).get(initTable[string, string]())
  let dbs = toSeq(configTable.keys).filter(k => k != "options")
  let defaultConfig = createConfigFromTable(options, dbs)

  if wasError:
    pacmanValidateAndThrow(("config", some(configFile), ArgumentType.long))

  proc getColor(color: string): ColorMode =
    let colors = toSeq(enumerate[ColorMode]())
    colors.filter(c => $c == color).optLast.get(ColorMode.colorNever)

  let root = getAll((some("r"), "root")).optLast.orElse(defaultConfig.rootOption)
  let db = getAll((some("b"), "dbpath")).optLast.orElse(defaultConfig.dbOption)
  let gpg = getAll((none(string), "gpgdir")).optLast.orElse(defaultConfig.gpgOption)
  let arch = getAll((none(string), "arch")).optLast.get(defaultConfig.arch)
  let colorStr = getAll((none(string), "color")).optLast.get($defaultConfig.colorMode)
  let color = getColor(colorStr)

  let debug = args.check((none(string), "debug"))
  let progressBar = not args.check((none(string), "noprogressbar"))
  let ignorePkgs = getAll((none(string), "ignore")).toSet
  let ignoreGroups = getAll((none(string), "ignoregroups")).toSet

  let hasKeyserver = forkWaitRedirect(() => (block:
    dropPrivileges()
    execResult(gpgConfCmd, "--list-options", "gpg")))
    .output
    .filter(s => s.len > 10 and s[0 .. 9] == "keyserver:" and not (s[^2] == ':'))
    .len > 0

  let pgpKeyserver = if hasKeyserver:
      none(string)
    else: (block:
      let argPgpKeyserver = getAll((none(string), "keyserver")).optLast
      if argPgpKeyserver.isSome:
        argPgpKeyserver
      else:
        var pgpKeyserver = none(string)
        var file: File
        if file.open(gpg.get(sysConfDir & "/pacman.d/gnupg") & "/gpg.conf"):
          try:
            while true:
              let line = file.readLine()
              if line.len > 10 and line[0 .. 9] == "keyserver ":
                pgpKeyserver = some(line[9 .. ^1].strip)
          except:
            discard
        pgpKeyserver)

  let config = PacmanConfig(rootOption: root, dbOption: db, gpgOption: gpg,
    dbs: defaultConfig.dbs, arch: arch, colorMode: color, debug: debug,
    progressBar: progressBar, verbosePkgList: defaultConfig.verbosePkgList,
    pgpKeyserver: pgpKeyserver, ignorePkgs: ignorePkgs + defaultConfig.ignorePkgs,
    ignoreGroups: ignoreGroups + defaultConfig.ignoreGroups)

  if config.dbs.find("aur") >= 0:
    raise commandError(tr"repo '$#' is reserved by this program" % ["aur"],
      colorNeeded = some(color.get))

  pacmanValidateAndThrow(("root", some(config.root), ArgumentType.long),
    ("dbpath", some(config.db), ArgumentType.long),
    ("arch", some(config.arch), ArgumentType.long),
    ("color", some(colorStr), ArgumentType.long))

  config

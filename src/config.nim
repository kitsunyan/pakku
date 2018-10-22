import
  future, options, posix, re, sequtils, sets, strutils, tables,
  utils

type
  ColorMode* {.pure.} = enum
    colorNever = "never",
    colorAuto = "auto",
    colorAlways = "always"

  PreserveBuilt* {.pure.} = enum
    internal = "Internal",
    user = "User",
    disabled = "Disabled"

  CommonConfig* = tuple[
    dbs: seq[string],
    arch: string,
    debug: bool,
    progressBar: bool,
    verbosePkgLists: bool,
    downloadTimeout: bool,
    pgpKeyserver: Option[string],
    defaultRoot: bool,
    ignorePkgs: HashSet[string],
    ignoreGroups: HashSet[string]
  ]

  PacmanConfig* = tuple[
    common: CommonConfig,
    sysrootOption: Option[string],
    rootRelOption: Option[string],
    dbRelOption: Option[string],
    cacheRelOption: Option[string],
    gpgRelOption: Option[string],
    colorMode: ColorMode
  ]

  Config* = tuple[
    common: CommonConfig,
    root: string,
    db: string,
    cache: string,
    userCacheInitial: string,
    userCacheCurrent: string,
    tmpRootInitial: string,
    tmpRootCurrent: string,
    color: bool,
    aurRepo: string,
    aurComments: bool,
    checkIgnored: bool,
    ignoreArch: bool,
    printAurNotFound: bool,
    printLocalIsNewer: bool,
    sudoExec: bool,
    viewNoDefault: bool,
    preserveBuilt: PreserveBuilt,
    preBuildCommand: Option[string]
  ]

proc readConfigFile*(configFile: string):
  (OrderedTable[string, ref Table[string, string]], bool) =
  var file: File
  var table = initOrderedTable[string, ref Table[string, string]]()
  var category: ref Table[string, string]
  var currentCategory = ""

  let wasError = if file.open(configFile):
      try:
        var matches: array[2, string]

        while true:
          let line = readLine(file).strip(leading = true, trailing = true)
          if line.len > 0 and line[0] != '#':
            if line.match(re"\[(.*)\]", matches):
              currentCategory = matches[0]
              if table.hasKey(currentCategory):
                category = table[currentCategory]
              else:
                category = newTable[string, string]()
                table[currentCategory] = category
            elif currentCategory.len > 0:
              if line.match(re"(\w+)\ *=\ *(.*)", matches):
                category[].add(matches[0], matches[1])
              else:
                category[].add(line, "")

        false
      except EOFError:
        false
      except IOError:
        true
      finally:
        file.close()
    else:
      true

  (table, wasError)

proc ignored*(config: Config, name: string, groups: openArray[string]): bool =
  name in config.common.ignorePkgs or (config.common.ignoreGroups * groups.toSet).len > 0

proc get*(colorMode: ColorMode): bool =
  case colorMode:
    of ColorMode.colorNever: false
    of ColorMode.colorAlways: true
    of ColorMode.colorAuto: isatty(1) == 1

proc pacmanRootRel*(config: PacmanConfig): string =
  config.rootRelOption.get("/")

proc pacmanDbRel*(config: PacmanConfig): string =
  if config.dbRelOption.isSome:
    config.dbRelOption.unsafeGet
  else:
    let root = config.pacmanRootRel
    let workRoot = if root == "/": "" else: root
    workRoot & localStateDir & "/lib/pacman/"

proc pacmanCacheRel*(config: PacmanConfig): string =
  config.cacheRelOption.get(localStateDir & "/cache/pacman/pkg")

proc simplifyConfigPath(path: string): string =
  if path.find("//") >= 0:
    simplifyConfigPath(path.replace("//", "/"))
  else:
    path

proc extendRel*(pathRel: string, sysroot: Option[string]): string =
  sysroot.map(s => (s & "/" & pathRel).simplifyConfigPath).get(pathRel)

proc obtainConfig*(config: PacmanConfig): Config =
  let (configTable, _) = readConfigFile(sysConfDir & "/pakku.conf")
  let options = configTable.opt("options").map(t => t[]).get(initTable[string, string]())

  let root = config.pacmanRootRel.extendRel(config.sysrootOption)
  let db = config.pacmanDbRel.extendRel(config.sysrootOption)
  let cache = config.pacmanCacheRel.extendRel(config.sysrootOption)
  let color = config.colorMode.get

  proc handleDirPattern(dirPattern: string, user: User): string =
    dirPattern
      .replace("${UID}", $user.uid)
      .replace("${USER}", user.name)
      .replace("${HOME}", user.home)
      .replace("$$", "$")

  proc obtainUserCacheDir(user: User): string =
    options.opt("UserCacheDir").get("${HOME}/.cache/pakku").handleDirPattern(user)

  proc obtainTmpDir(user: User): string =
    options.opt("TmpDir").get("/tmp/pakku-${USER}").handleDirPattern(user)

  let initialOrCurrentUser = initialUser.get(currentUser)
  let userCacheInitial = obtainUserCacheDir(initialOrCurrentUser)
  let userCacheCurrent = obtainUserCacheDir(currentUser)
  let tmpRootInitial = obtainTmpDir(initialOrCurrentUser)
  let tmpRootCurrent = obtainTmpDir(currentUser)
  let aurRepo = options.opt("AurRepo").get("aur")
  let aurComments = options.hasKey("AurComments")
  let checkIgnored = options.hasKey("CheckIgnored")
  let ignoreArch = options.hasKey("IgnoreArch")
  let printAurNotFound = options.hasKey("PrintAurNotFound")
  let printLocalIsNewer = options.hasKey("PrintLocalIsNewer")
  let sudoExec = options.hasKey("SudoExec")
  let viewNoDefault = options.hasKey("ViewNoDefault")
  let preserveBuilt = toSeq(enumerate[PreserveBuilt]())
    .filter(o => some($o) == options.opt("PreserveBuilt"))
    .optLast.get(PreserveBuilt.disabled)
  let preBuildCommand = options.opt("PreBuildCommand")

  if config.common.dbs.find(aurRepo) >= 0:
    raise commandError(tr"repo '$#' can not be used as fake AUR repository" % [aurRepo],
      colorNeeded = some(color))

  if aurRepo.find('/') >= 0:
    raise commandError(trp("could not register '%s' database (%s)\n") %
      [aurRepo, tra"wrong or NULL argument passed"], colorNeeded = some(color))

  ((config.common.dbs, config.common.arch, config.common.debug, config.common.progressBar,
    config.common.verbosePkgLists, config.common.downloadTimeout, config.common.pgpKeyserver,
    config.common.defaultRoot and config.sysrootOption.isNone,
    config.common.ignorePkgs, config.common.ignoreGroups),
    root, db, cache, userCacheInitial, userCacheCurrent, tmpRootInitial, tmpRootCurrent,
    color, aurRepo, aurComments, checkIgnored, ignoreArch, printAurNotFound, printLocalIsNewer,
    sudoExec, viewNoDefault, preserveBuilt, preBuildCommand)

template withAlpmConfig*(config: Config, passDbs: bool,
  handle: untyped, alpmDbs: untyped, errors: untyped, body: untyped): untyped =
  withAlpm(config.root, config.db, if passDbs: config.common.dbs else: @[],
    config.common.arch, handle, alpmDbs, errors, body)

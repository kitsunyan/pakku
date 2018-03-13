import
  future, options, posix, re, sets, strutils, tables,
  utils

type
  ColorMode* {.pure.} = enum
    colorNever = "never",
    colorAuto = "auto",
    colorAlways = "always"

  CommonConfig* = object of RootObj
    dbs*: seq[string]
    arch*: string
    debug*: bool
    progressBar*: bool
    verbosePkgList*: bool
    ignorePkgs*: HashSet[string]
    ignoreGroups*: HashSet[string]

  PacmanConfig* = object of CommonConfig
    rootOption*: Option[string]
    dbOption*: Option[string]
    colorMode*: ColorMode

  Config* = object of CommonConfig
    root*: string
    db*: string
    tmpRoot*: string
    color*: bool
    aurComments*: bool
    checkIgnored*: bool
    printAurNotFound*: bool
    viewNoDefault*: bool

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
          let rawLine = readLine(file).strip(leading = false, trailing = true)
          let commentIndex = rawLine.find('#')
          let line = if commentIndex >= 0:
              rawLine[0 .. commentIndex - 1].strip(leading = false, trailing = true)
            else:
              rawLine

          if line.len > 0:
            if line.match(re"\[(.*)\]", matches):
              currentCategory = matches[0]
              if table.hasKey(currentCategory):
                category = table[currentCategory]
              else:
                category = newTable[string, string]()
                table[currentCategory] = category
            elif currentCategory.len > 0:
              if line.match(re"\ *(\w+)\ *=\ *(.*)", matches):
                category[].add(matches[0], matches[1])
              else:
                category[].add(line.strip(leading = true, trailing = false), "")

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
  name in config.ignorePkgs or (config.ignoreGroups * groups.toSet).len > 0

proc isRootDefault*(config: Config): bool =
  config.root == "/"

proc get*(colorMode: ColorMode): bool =
  case colorMode:
    of ColorMode.colorNever: false
    of ColorMode.colorAlways: true
    of ColorMode.colorAuto: isatty(1) == 1

proc root*(config: PacmanConfig): string =
  config.rootOption.get("/")

proc db*(config: PacmanConfig): string =
  if config.dbOption.isSome:
    config.dbOption.unsafeGet
  else:
    let root = config.root
    let workRoot = if root == "/": "" else: root
    workRoot & localStateDir & "/lib/pacman/"

proc obtainConfig*(config: PacmanConfig): Config =
  let (configTable, _) = readConfigFile(sysConfDir & "/pakku.conf")
  let options = configTable.opt("options").map(t => t[]).get(initTable[string, string]())

  let root = config.root
  let db = config.db
  let color = config.colorMode.get

  let (userId, userName) = getUser()
  let tmpRoot = options.opt("TmpDir").get("/tmp/pakku-${USER}")
    .replace("${UID}", $userId)
    .replace("${USER}", userName)
  let aurComments = options.hasKey("AurComments")
  let checkIgnored = options.hasKey("CheckIgnored")
  let printAurNotFound = options.hasKey("PrintAurNotFound")
  let viewNoDefault = options.hasKey("ViewNoDefault")

  Config(root: root, db: db, tmpRoot: tmpRoot, color: color,
    dbs: config.dbs, arch: config.arch, debug: config.debug,
    progressBar: config.progressBar, verbosePkgList: config.verbosePkgList,
    ignorePkgs: config.ignoreGroups, ignoreGroups: config.ignoreGroups,
    aurComments: aurComments, checkIgnored: checkIgnored,
    printAurNotFound: printAurNotFound, viewNoDefault: viewNoDefault)
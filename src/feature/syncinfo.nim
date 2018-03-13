import
  future, options, posix, sequtils, strutils, times,
  "../args", "../aur", "../common", "../config", "../format", "../package",
    "../pacman", "../utils",
  "../wrapper/alpm"

const
  pacmanInfoStrings = [
    "Architecture",
    "Backup Files",
    "Build Date",
    "Compressed Size",
    "Conflicts With",
    "Depends On",
    "Description",
    "Download Size",
    "Groups",
    "Install Date",
    "Install Reason",
    "Install Script",
    "Installed Size",
    "Licenses",
    "MD5 Sum",
    "Name",
    "Optional Deps",
    "Optional For",
    "Packager",
    "Provides",
    "Replaces",
    "Repository",
    "Required By",
    "SHA-256 Sum",
    "Signatures",
    "URL",
    "Validated By",
    "Version"
  ]

proc formatDeps(title: string, config: Config,
  refs: seq[PackageReference]): PackageLineFormat =
  let values: seq[tuple[title: string, hasDesc: bool]] = refs
    .map(r => r.description
      .map(d => ($r & ": " & d, true))
      .get(($r, false)))

  if values.len > 0:
    (title, values.map(v => v.title), values.map(v => v.hasDesc).foldl(a or b))
  else:
    (title, @[], false)

proc formatDate(date: Option[int64]): seq[string] =
  if date.isSome:
    var time = (posix.Time) date.unsafeGet
    var ltime: Tm
    discard localtime_r(time, ltime)
    var buffer: array[100, char]
    let res = strftime(addr(buffer), buffer.len, "%c", ltime)
    if res > 0: @[buffer.toString(none(int))] else: @[]
  else:
    @[]

proc handleTarget(config: Config, padding: int, args: seq[Argument],
  target: FullPackageTarget[PackageInfo]): int =
  if target.foundInfo.isSome:
    if isAurTargetFull[PackageInfo](target):
      let pkgInfo = target.pkgInfo.unsafeGet

      printPackageInfo(padding, config.color,
        (trp"Repository", @["aur"], false),
        (trp"Name", @[pkgInfo.name], false),
        (trp"Version", @[pkgInfo.version], false),
        (trp"Description", toSeq(pkgInfo.description.items), false),
        (trp"Architecture", pkgInfo.archs, false),
        (trp"URL", toSeq(pkgInfo.url.items), false),
        (trp"Licenses", pkgInfo.licenses, false),
        (trp"Groups", pkgInfo.groups, false),
        formatDeps(trp"Provides", config, pkgInfo.provides),
        formatDeps(trp"Depends On", config, pkgInfo.depends),
        formatDeps(trp"Optional Deps", config, pkgInfo.optional),
        formatDeps(trp"Conflicts With", config, pkgInfo.conflicts),
        formatDeps(trp"Replaces", config, pkgInfo.replaces),
        (tr"Maintainer", toSeq(pkgInfo.maintainer.items()), false),
        (tr"First Submitted", pkgInfo.firstSubmitted.formatDate, false),
        (tr"Last Modified", pkgInfo.lastModified.formatDate, false),
        (tr"Rating", @[formatPkgRating(pkgInfo.votes, pkgInfo.popularity)], false))

      0
    else:
      pacmanRun(false, config.color, args &
        (target.formatArgument, none(string), ArgumentType.target))
  else:
    if target.repo == some("aur"):
      printError(config.color, trp("package '%s' was not found\n") % [target.formatArgument])
      1
    else:
      pacmanRun(false, config.color, args &
        (target.formatArgument, none(string), ArgumentType.target))

proc handleSyncInfo*(args: seq[Argument], config: Config): int =
  let (_, callArgs) = checkAndRefresh(config.color, args)
  let targets = args.packageTargets

  let (syncTargets, checkAur) = withAlpm(config.root, config.db,
    config.dbs, config.arch, handle, dbs, errors):
    for e in errors: printError(config.color, e)
    findSyncTargets(handle, dbs, targets, false, false)

  let (pkgInfos, aerrors) = getAurPackageInfo(checkAur, none(seq[RpcPackageInfo]),
    config.arch, proc (a: int, b: int) = discard)
  for e in aerrors: printError(config.color, e)

  let fullTargets = mapAurTargets[PackageInfo](syncTargets, pkgInfos)

  let code = min(aerrors.len, 1)
  if fullTargets.filter(isAurTargetFull[PackageInfo]).len == 0:
    if code == 0:
      pacmanExec(false, config.color, callArgs)
    else:
      discard pacmanRun(false, config.color, callArgs)
      code
  else:
    let finalArgs = callArgs.filter(arg => not arg.isTarget)
    let padding = pacmanInfoStrings.map(s => s.trp).computeMaxLength

    let codes = code & lc[handleTarget(config, padding, finalArgs, x) | (x <- fullTargets), int]
    codes.filter(c => c != 0).optFirst.get(0)

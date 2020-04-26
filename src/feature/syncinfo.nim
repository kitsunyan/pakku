import
  options, posix, sequtils, strutils, sugar, tables,
  "../args", "../aur", "../common", "../config", "../format", "../lc", "../package",
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

proc formatDate(date: int64): Option[string] =
  var time = (posix.Time) date
  var ltime: Tm
  discard localtime_r(time, ltime)
  var buffer: array[100, char]
  let res = strftime(addr(buffer), buffer.len, "%c", ltime)
  if res > 0: some(buffer.toString(none(int))) else: none(string)

proc handleTarget(config: Config, padding: int, args: seq[Argument],
  full: FullPackageTarget, pkgInfoOption: Option[PackageInfo]): int =
  if full.sync.foundInfos.len > 0:
    if full.isAurTargetFull(config.aurRepo):
      let pkgInfo = pkgInfoOption.get

      printPackageInfo(padding, config.color,
        (trp"Repository", @[config.aurRepo], false),
        (trp"Name", @[pkgInfo.rpc.name], false),
        (trp"Version", @[pkgInfo.rpc.version], false),
        (trp"Description", toSeq(pkgInfo.rpc.description.items), false),
        (trp"Architecture", pkgInfo.archs, false),
        (trp"URL", toSeq(pkgInfo.url.items), false),
        (trp"Licenses", pkgInfo.licenses, false),
        (trp"Groups", pkgInfo.groups, false),
        formatDeps(trp"Provides", config, pkgInfo.provides),
        formatDeps(trp"Depends On", config, pkgInfo.depends),
        formatDeps(trp"Optional Deps", config, pkgInfo.optional),
        formatDeps(trp"Conflicts With", config, pkgInfo.conflicts),
        formatDeps(trp"Replaces", config, pkgInfo.replaces),
        (tr"Maintainer", toSeq(pkgInfo.rpc.maintainer.items()), false),
        (tr"First Submitted", toSeq(pkgInfo.rpc.firstSubmitted
          .map(formatDate).flatten.items()), false),
        (tr"Last Modified", toSeq(pkgInfo.rpc.lastModified
          .map(formatDate).flatten.items()), false),
        (tr"Out Of Date", toSeq(pkgInfo.rpc.outOfDate
          .map(formatDate).flatten.items()), false),
        (tr"Rating", @[formatPkgRating(pkgInfo.rpc.votes, pkgInfo.rpc.popularity)], false))

      0
    elif full.sync.target.reference.constraint.isSome:
      # pacman doesn't support constraints for --info queries
      pacmanRun(false, config.color, args & full.sync.foundInfos.map(i =>
        (i.repo & "/" & full.sync.target.reference.name, none(string), ArgumentType.target)))
    else:
      pacmanRun(false, config.color, args &
        ($full.sync.target, none(string), ArgumentType.target))
  else:
    if full.sync.target.repo == some(config.aurRepo):
      printError(config.color, trp("package '%s' was not found\n") % [$full.sync.target])
      1
    else:
      pacmanRun(false, config.color, args & ($full.sync.target, none(string), ArgumentType.target))

proc handleSyncInfo*(args: seq[Argument], config: Config): int =
  let (refreshCode, callArgs) = checkAndRefresh(config.color, args)
  if refreshCode != 0:
    refreshCode
  else:
    let targets = args.packageTargets(false)

    let (syncTargets, checkAurNames) = withAlpmConfig(config, true, handle, dbs, errors):
      for e in errors: printError(config.color, e)
      findSyncTargets(handle, dbs, targets, config.aurRepo, false, false)

    let (pkgInfos, _, aerrors) = getAurPackageInfos(checkAurNames,
      config.aurRepo, config.common.arch, config.common.downloadTimeout)
    for e in aerrors: printError(config.color, e)

    let fullTargets = mapAurTargets(syncTargets, pkgInfos.map(p => p.rpc), config.aurRepo)

    let code = min(aerrors.len, 1)
    if fullTargets.filter(f => f.isAurTargetFull(config.aurRepo) or
      f.sync.target.repo == some(config.aurRepo) or
      f.sync.target.reference.constraint.isSome).len == 0:
      if code == 0:
        pacmanExec(false, config.color, callArgs)
      else:
        discard pacmanRun(false, config.color, callArgs)
        code
    else:
      let finalArgs = callArgs.filter(arg => not arg.isTarget)
      let padding = pacmanInfoStrings.map(s => s.trp).computeMaxLength
      let pkgInfosTable = pkgInfos.map(i => (i.rpc.toPackageReference, i)).toTable

      let codes = code & lc[handleTarget(config, padding, finalArgs, x,
        x.rpcInfo.map(i => pkgInfosTable.opt(i.toPackageReference)).flatten) |
        (x <- fullTargets), int]
      codes.filter(c => c != 0).optFirst.get(0)

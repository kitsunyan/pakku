import
  algorithm, future, options, sequtils, strutils,
  "../args", "../aur", "../config", "../common", "../format", "../package",
    "../pacman", "../utils",
  "../wrapper/alpm"

proc handleSyncSearch*(args: seq[Argument], config: Config): int =
  let (refreshCode, callArgs) = checkAndRefresh(config.color, args)
  if refreshCode != 0:
    refreshCode
  else:
    let quiet = args.check(%%%"quiet")

    let (aurPackages, aerrors) = findAurPackages(args.targets, config.aurRepo)
    for e in aerrors: printError(config.color, e)

    type Package = tuple[rpcInfo: RpcPackageInfo, installedVersion: Option[string]]

    proc checkLocalPackages: seq[Package] =
      if quiet:
        aurPackages.map(pkg => (pkg, none(string)))
      elif aurPackages.len > 0:
        withAlpmConfig(config, false, handle, dbs, errors):
          for e in errors: printError(config.color, e)

          aurPackages.map(proc (rpcInfo: RpcPackageInfo): Package =
            let pkg = handle.local[rpcInfo.name]
            if pkg != nil:
              (rpcInfo, some($pkg.version))
            else:
              (rpcInfo, none(string)))
      else:
        @[]

    let pkgs = checkLocalPackages()
      .sorted((a, b) => cmp(a.rpcInfo.name, b.rpcInfo.name))

    var code = min(aerrors.len, 1)
    if pkgs.len == 0:
      if code == 0:
        pacmanExec(false, config.color, callArgs)
      else:
        discard pacmanRun(false, config.color, callArgs)
        code
    else:
      discard pacmanRun(false, config.color, callArgs)

      for pkg in pkgs:
        if quiet:
          echo(pkg.rpcInfo.name)
        else:
          printPackageSearch(config.color, config.aurRepo, pkg.rpcInfo.name,
            pkg.rpcInfo.version, pkg.installedVersion, pkg.rpcInfo.description,
            some(formatPkgRating(pkg.rpcInfo.votes, pkg.rpcInfo.popularity)))
      0

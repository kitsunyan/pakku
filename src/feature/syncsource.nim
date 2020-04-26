import
  options, os, posix, sequtils, strutils, sugar, tables,
  "../args", "../aur", "../common", "../config", "../format", "../lists",
    "../package", "../pacman", "../utils",
  "../wrapper/alpm"

type
  BaseTarget = tuple[
    base: string,
    version: string,
    destination: string,
    aurGitUrl: Option[string],
    gitRepo: Option[GitRepo]
  ]

  CloneResult = tuple[
    base: string,
    path: string,
    files: seq[string],
    destination: string
  ]

proc getFilesOrClear(base: string, repoPath: string, gitSubdir: Option[string]):
  (seq[string], Option[string]) =
  let rawFiles = getGitFiles(repoPath, gitSubdir, false)
    .filter(f => f != ".gitignore" and f != ".SRCINFO" and f.find('/') < 0)
    .map(f => gitSubdir.map(s => s & "/" & f).get(f))

  if rawFiles.len > 0:
    (rawFiles, none(string))
  else:
    removeDirQuiet(repoPath)
    (newSeq[string](), some(tr"$#: failed to clone git repository" % [base]))

proc cloneRepositories(config: Config, targets: seq[BaseTarget],
  update: (int, int) -> void): (List[CloneResult], seq[string]) =
  let (bcount, berrors) = cloneBareRepos(config, BareKind.repo,
    targets.filter(t => t.gitRepo.isSome).map(t => t.gitRepo.unsafeGet),
    proc (progress: int, count: int) = update(progress, count + targets.len), false)

  proc cloneNext(index: int, results: List[CloneResult], messages: List[string]):
    (List[CloneResult], List[string]) =
    update(bcount + index, bcount + targets.len)

    if index >= targets.len:
      (results.reversed, messages.reversed)
    else:
      let target = targets[index]
      let repoPath = repoPath(config.tmpRootCurrent, target.base)
      removeDirQuiet(repoPath)

      if target.aurGitUrl.isSome:
        let (cloneCode, cerror) = cloneAurRepo(config,
          target.base, target.aurGitUrl.unsafeGet, false)

        if cloneCode != 0:
          cloneNext(index + 1, results, toSeq(cerror.items) ^& messages)
        else:
          let (files, ferror) = getFilesOrClear(target.base, repoPath, none(string))
          if ferror.isSome:
            cloneNext(index + 1, results, ferror.unsafeGet ^& messages)
          else:
            cloneNext(index + 1, (target.base, repoPath, files,
              target.destination) ^& results, messages)
      elif target.gitRepo.isSome:
        let gitRepo = target.gitRepo.unsafeGet
        let cerror = clonePackageRepo(config, target.base,
          target.version, gitRepo, false)

        if cerror.isSome:
          cloneNext(index + 1, results, cerror.unsafeGet ^& messages)
        else:
          let (files, ferror) = getFilesOrClear(target.base, repoPath, some(gitRepo.path))
          if ferror.isSome:
            cloneNext(index + 1, results, ferror.unsafeGet ^& messages)
          else:
            cloneNext(index + 1, (target.base, repoPath, files,
              target.destination) ^& results, messages)
      else:
        let message = tr"$#: repository not found" % [target.base]
        cloneNext(index + 1, results, message ^& messages)

  if berrors.len > 0:
    (nil, berrors)
  else:
    let (results, cerrors) = cloneNext(0, nil, nil)
    (results, toSeq(cerrors.items))

proc copyFiles(config: Config, quiet: bool, results: seq[CloneResult]): List[string] =
  proc copyNext(index: int, messages: List[string]): List[string] =
    if index >= results.len:
      messages.reversed
    else:
      let res = results[index]
      discard mkdir(res.destination, 0o755)

      let error = try:
        for f in res.files:
          let index = f.rfind('/')
          let name = if index >= 0: f[index + 1 .. ^1] else: f
          let dest = if res.destination == ".": name else: res.destination & "/" & name
          moveFile(res.path & "/" & f, dest)
          printFile(config.color, quiet, res.base, dest)
        none(string)
      except OSError:
        some(tr"$#: failed to move files" % [res.base])

      copyNext(index + 1, toSeq(error.items) ^& messages)

  copyNext(0, nil)

proc cloneAndCopy(config: Config, quiet: bool, fullTargets: seq[FullPackageTarget]): int =
  let baseTargets = fullTargets.foldl(block:
    let bases = a.map(x => x.base)
    if b.isAurTargetFull(config.aurRepo):
      let rpcInfo = b.rpcInfo.get
      if rpcInfo.base in bases:
        a
      else:
        a & (rpcInfo.base, rpcInfo.version, b.sync.target.destination.get(rpcInfo.base),
          some(rpcInfo.gitUrl), none(GitRepo))
    else:
      let foundInfo = b.sync.foundInfos[0]
      let pkg = foundInfo.pkg.get
      if pkg.base in bases:
        a
      else:
        let git = lookupGitRepo(foundInfo.repo, pkg.base, pkg.arch.get)
        a & (pkg.base, pkg.version, b.sync.target.destination.get(pkg.base), none(string), git),
    newSeq[BaseTarget]())

  let (update, terminate) = if quiet:
      (proc (a: int, b: int) {.closure, sideEffect.} = discard,
        proc () {.closure, sideEffect.} = discard)
    else:
      printProgressShare(config.common.progressBar, tr"cloning repositories")

  let (results, rerrors) = cloneRepositories(config, baseTargets, update)
  terminate()
  for e in rerrors: printError(config.color, e)

  let cerrors = copyFiles(config, quiet, toSeq(results.items))
  for e in cerrors: printError(config.color, e)

  for result in results:
    removeDirQuiet(result.path)
  discard rmdir(config.tmpRootCurrent)

  if rerrors.len > 0 or cerrors != nil:
    1
  else:
    0

proc handleSyncSource*(args: seq[Argument], config: Config): int =
  let (refreshCode, _) = checkAndRefresh(config.color, args)
  if refreshCode != 0:
    refreshCode
  else:
    let quiet = args.check(%%%"quiet")
    let targets = args.packageTargets(true)

    if targets.len == 0:
      printError(config.color, trp("no targets specified (use -h for help)\n"))
      1
    else:
      let (syncTargets, checkAurNames) = withAlpmConfig(config, true, handle, dbs, errors):
        for e in errors: printError(config.color, e)
        findSyncTargets(handle, dbs, targets, config.aurRepo, false, false)

      let (rpcInfos, aerrors) = getRpcPackageInfos(checkAurNames,
        config.aurRepo, config.common.downloadTimeout)
      for e in aerrors: printError(config.color, e)

      let notFoundTargets = filterNotFoundSyncTargets(syncTargets,
        rpcInfos, initTable[string, PackageReference](), config.aurRepo)

      if notFoundTargets.len > 0:
        printSyncNotFound(config, notFoundTargets)
        1
      else:
        let fullTargets = mapAurTargets(syncTargets, rpcInfos, config.aurRepo)
        cloneAndCopy(config, quiet, fullTargets)

import
  future, options, os, osproc, posix, sequtils, sets, strutils, tables,
  args, config, package, pacman, utils,
  "wrapper/alpm"

type
  SyncFoundPackageInfo* = tuple[
    base: string,
    version: string,
    arch: Option[string]
  ]

  SyncFoundInfo* = tuple[
    repo: string,
    pkg: Option[SyncFoundPackageInfo]
  ]

  PackageTarget* = object of RootObj
    name*: string
    repo*: Option[string]

  SyncPackageTarget* = object of PackageTarget
    foundInfo*: Option[SyncFoundInfo]

  FullPackageTarget*[T] = object of SyncPackageTarget
    pkgInfo*: Option[T]

proc toPackageReference*(dependency: ptr AlpmDependency): PackageReference =
  let op = case dependency.depmod:
    of AlpmDepMod.eq: some(ConstraintOperation.eq)
    of AlpmDepMod.ge: some(ConstraintOperation.ge)
    of AlpmDepMod.le: some(ConstraintOperation.le)
    of AlpmDepMod.gt: some(ConstraintOperation.gt)
    of AlpmDepMod.lt: some(ConstraintOperation.lt)
    else: none(ConstraintOperation)

  let description = if dependency.desc != nil: some($dependency.desc) else: none(string)
  ($dependency.name, description, op.map(o => (o, $dependency.version)))

proc checkConstraints(lop: ConstraintOperation, rop: ConstraintOperation, cmp: int): bool =
  let (x1, x2) = if cmp > 0:
      (1, -1)
    elif cmp < 0:
      (-1, 1)
    else:
      (0, 0)

  proc c(op: ConstraintOperation, x1: int, x2: int): bool =
    case op:
      of ConstraintOperation.eq: x1 == x2
      of ConstraintOperation.ge: x1 >= x2
      of ConstraintOperation.le: x1 <= x2
      of ConstraintOperation.gt: x1 > x2
      of ConstraintOperation.lt: x1 < x2

  template a(x: int): bool = lop.c(x, x1) and rop.c(x, x2)

  a(2) or a(1) or a(0) or a(-1) or a(-2)

proc isProvidedBy*(package: PackageReference, by: PackageReference): bool =
  if package.name == by.name:
    if package.constraint.isNone or by.constraint.isNone:
      true
    else:
      let lcon = package.constraint.unsafeGet
      let rcon = package.constraint.unsafeGet
      let cmp = vercmp(lcon.version, rcon.version)
      checkConstraints(lcon.operation, rcon.operation, cmp)
  else:
    false

proc checkAndRefresh*(color: bool, args: seq[Argument]): tuple[code: int, args: seq[Argument]] =
  let refreshCount = args.count((some("y"), "refresh"))
  if refreshCount > 0:
    let code = pacmanRun(true, color, args
      .keepOnlyOptions(commonOptions, upgradeCommonOptions) &
      ("S", none(string), ArgumentType.short) &
      ("y", none(string), ArgumentType.short).repeat(refreshCount))

    let callArgs = args
      .filter(arg => not arg.matchOption((some("y"), "refresh")))
    (code, callArgs)
  else:
    (0, args)

proc packageTargets*(args: seq[Argument]): seq[PackageTarget] =
  args.targets.map(proc (target: string): PackageTarget =
    let splitTarget = target.split('/', 2)
    if splitTarget.len == 2:
      PackageTarget(name: splitTarget[1], repo: some(splitTarget[0]))
    else:
      PackageTarget(name: target, repo: none(string)))

proc isAurTargetSync*(target: SyncPackageTarget): bool =
  target.foundInfo.isNone and (target.repo.isNone or target.repo == some("aur"))

proc isAurTargetFull*[T: RpcPackageInfo](target: FullPackageTarget[T]): bool =
  target.foundInfo.isSome and target.foundInfo.unsafeGet.repo == "aur"

proc findSyncTargets*(handle: ptr AlpmHandle, dbs: seq[ptr AlpmDatabase],
  targets: seq[PackageTarget], allowGroups: bool, checkProvides: bool):
  (seq[SyncPackageTarget], seq[string]) =
  let dbTable = dbs.map(d => ($d.name, d)).toTable

  proc checkProvided(name: string, db: ptr AlpmDatabase): bool =
    for pkg in db.packages:
      for provides in pkg.provides:
        if $provides.name == name:
          return true
    return false

  proc findSync(target: PackageTarget): Option[SyncFoundInfo] =
    if target.repo.isSome:
      let repo = target.repo.unsafeGet

      if dbTable.hasKey(repo):
        let db = dbTable[repo]
        let pkg = db[target.name]

        if pkg != nil:
          let base = if pkg.base == nil: target.name else: $pkg.base
          return some((repo, some((base, $pkg.version, some($pkg.arch)))))
        elif checkProvides and target.name.checkProvided(db):
          return some((repo, none(SyncFoundPackageInfo)))
        else:
          return none(SyncFoundInfo)
      else:
        return none(SyncFoundInfo)
    else:
      let directResult = dbs
        .map(db => (block:
          let pkg = db[target.name]
          if pkg != nil:
            let base = if pkg.base == nil: target.name else: $pkg.base
            some(($db.name, some((base, $pkg.version, some($pkg.arch)))))
          else:
            none(SyncFoundInfo)))
        .filter(i => i.isSome)
        .optFirst
        .flatten

      if directResult.isSome:
        return directResult
      else:
        if allowGroups:
          let groupRepo = lc[d | (d <- dbs, g <- d.groups, $g.name == target.name),
            ptr AlpmDatabase].optFirst
          if groupRepo.isSome:
            return groupRepo.map(d => ($d.name, none(SyncFoundPackageInfo)))

        if checkProvides:
          for db in dbs:
            if target.name.checkProvided(db):
              return some(($db.name, none(SyncFoundPackageInfo)))
          return none(SyncFoundInfo)
        else:
          return none(SyncFoundInfo)

  let syncTargets = targets.map(t => SyncPackageTarget(name: t.name,
    repo: t.repo, foundInfo: findSync(t)))
  let checkAur = syncTargets.filter(isAurTargetSync).map(t => t.name)
  (syncTargets, checkAur)

proc mapAurTargets*[T: RpcPackageInfo](targets: seq[SyncPackageTarget],
  pkgInfos: seq[T]): seq[FullPackageTarget[T]] =
  let aurTable = pkgInfos.map(i => (i.name, i)).toTable

  targets.map(proc (target: SyncPackageTarget): FullPackageTarget[T] =
    if target.foundInfo.isNone and aurTable.hasKey(target.name):
      let pkgInfo = aurTable[target.name]
      let syncInfo = ("aur", some((pkgInfo.base, pkgInfo.version, none(string))))
      FullPackageTarget[T](name: target.name, repo: target.repo,
        foundInfo: some(syncInfo), pkgInfo: some(pkgInfo))
    else:
      FullPackageTarget[T](name: target.name, repo: target.repo,
        foundInfo: target.foundInfo, pkgInfo: none(T)))

proc formatArgument*(target: PackageTarget): string =
  target.repo.map(r => r & "/" & target.name).get(target.name)

proc ensureTmpOrError*(config: Config): Option[string] =
  let tmpRootExists = try:
    discard config.tmpRoot.existsOrCreateDir()
    true
  except:
    false

  if not tmpRootExists:
    some(tr"failed to create tmp directory '$#'" % [config.tmpRoot])
  else:
    none(string)

proc bisectVersion(repoPath: string, debug: bool, firstCommit: Option[string],
  compareMethod: string, relativePath: string, version: string): Option[string] =
  template forkExecWithoutOutput(args: varargs[string]): int =
    forkWait(() => (block:
      discard close(0)
      if not debug:
        discard close(1)
        discard close(2)

      execResult(args)))

  let (workFirstCommit, checkFirst) = if firstCommit.isSome:
      (firstCommit, false)
    else:
      (runProgram(gitCmd, "-C", repoPath,
        "rev-list", "--max-parents=0", "@").optLast, true)
  let realLastThreeCommits = runProgram(gitCmd, "-C", repoPath,
    "rev-list", "--max-count=3", "@")
  let index = workFirstCommit.map(c => realLastThreeCommits.find(c)).get(-1)
  let lastThreeCommits = if index >= 0:
      realLastThreeCommits[0 .. index]
    else:
      realLastThreeCommits

  proc checkCommit(commit: string): Option[string] =
    let checkout1Code = forkExecWithoutOutput(gitCmd, "-C", repoPath,
      "checkout", commit)

    if checkout1Code != 0:
      none(string)
    else:
      let foundVersion = runProgram(pkgLibDir & "/bisect",
        compareMethod, repoPath & "/" & relativePath, version).optFirst
      let checkout2Code = forkExecWithoutOutput(gitCmd, "-C", repoPath,
        "checkout", lastThreeCommits[0])

      if checkout2Code != 0:
        none(string)
      elif foundVersion == some(version):
        some(commit)
      else:
        none(string)

  if lastThreeCommits.len == 0:
    none(string)
  elif lastThreeCommits.len == 1:
    if checkFirst:
      checkCommit(lastThreeCommits[0])
    else:
      none(string)
  elif lastThreeCommits.len == 2:
    let checkedCommit = checkCommit(lastThreeCommits[0])
    if checkedCommit.isSome:
      checkedCommit
    elif checkFirst:
      checkCommit(lastThreeCommits[1])
    else:
      none(string)
  else:
    # find the commit with specific package version using git bisect
    let bisectStartCode = forkExecWithoutOutput(gitCmd, "-C", repoPath,
      "bisect", "start", "@", workFirstCommit.get(""))

    if bisectStartCode != 0:
      none(string)
    else:
      discard forkExecWithoutOutput(gitCmd, "-C", repoPath,
        "bisect", "run", pkgLibDir & "/bisect", compareMethod, relativePath, version)

      let commit = runProgram(gitCmd, "-C", repoPath,
        "rev-list", "--max-count=1", "refs/bisect/bad").optFirst

      discard forkExecWithoutOutput(gitCmd, "-C", repoPath,
        "bisect", "reset")

      if commit.isSome:
        let checkedCommit = commit.map(checkCommit).flatten
        if checkedCommit.isSome:
          checkedCommit
        else:
          # non-incremental git history (e.g. downgrade without epoch change), bisect again
          bisectVersion(repoPath, debug, commit, compareMethod, relativePath, version)
      elif checkFirst and workFirstCommit.isSome:
        checkCommit(workFirstCommit.unsafeGet)
      else:
        none(string)

proc obtainBuildPkgInfos*(config: Config,
  pacmanTargets: seq[FullPackageTarget[RpcPackageInfo]]): (seq[PackageInfo], seq[string]) =
  type
    LookupBaseGroup = tuple[
      base: string,
      version: string,
      arch: string,
      repo: string
    ]

    LookupGitResult = tuple[
      group: LookupBaseGroup,
      git: Option[GitRepo]
    ]

  let bases: seq[LookupBaseGroup] = pacmanTargets
    .map(target => (block:
      let info = target.foundInfo.get
      let pkg = info.pkg.get
      (pkg.base, pkg.version, pkg.arch.get, info.repo)))
    .deduplicate

  let lookupResults: seq[LookupGitResult] = bases
    .map(b => (b, lookupGitRepo(b.repo, b.base, b.arch)))
  let notFoundRepos = lookupResults.filter(r => r.git.isNone)

  if notFoundRepos.len > 0:
    let messages = notFoundRepos.map(r => tr"$#: repository not found" % [r.group.base])
    (newSeq[PackageInfo](), messages)
  else:
    let message = ensureTmpOrError(config)
    if message.isSome:
      (@[], @[message.unsafeGet])
    else:
      proc findCommitAndGetSrcInfo(base: string, version: string,
        repo: string, git: GitRepo): seq[PackageInfo] =
        let repoPath = repoPath(config.tmpRoot, base)
        removeDirQuiet(repoPath)

        try:
          if forkWait(() => execResult(gitCmd, "-C", config.tmpRoot,
            "clone", "-q", git.url, "-b", git.branch,
            "--single-branch", base)) == 0:
            let commit = bisectVersion(repoPath, config.debug, none(string),
              "source", git.path, version)

            if commit.isNone:
              @[]
            else:
              discard forkWait(() => execResult(gitCmd, "-C", repoPath,
                "checkout", "-q", commit.unsafeGet))
              let output = execProcess(bashCmd, ["-c",
                """cd "$2/$3" && "$1" --printsrcinfo""",
                "bash", makePkgCmd, repoPath, git.path], options = {})
              parseSrcInfo(repo, output, git.url, some(git.branch), commit, some(git.path))
                .filter(i => i.version == version)
          else:
            @[]
        finally:
          removeDirQuiet(repoPath)

      let pkgInfos = lc[x | (r <- lookupResults, x <- findCommitAndGetSrcInfo(r.group.base,
        r.group.version, r.group.repo, r.git.get)), PackageInfo]

      let pkgInfosTable = pkgInfos.map(i => (i.name, i)).toTable

      let foundPkgInfos = lc[x | (y <- pacmanTargets,
        x <- pkgInfosTable.opt(y.name)), PackageInfo]
      let messages = pacmanTargets
        .filter(t => not pkgInfosTable.hasKey(t.name))
        .map(t => tr"$#: failed to get package info" % [t.name])

      discard rmdir(config.tmpRoot)
      (foundPkgInfos, messages)

proc cloneRepo*(config: Config, basePackages: seq[PackageInfo]): (int, Option[string]) =
  let base = basePackages[0].base
  let repoPath = repoPath(config.tmpRoot, base)

  let message = ensureTmpOrError(config)
  if message.isSome:
    (1, message)
  elif repoPath.existsDir():
    (0, none(string))
  else:
    let gitUrl = basePackages[0].gitUrl
    let gitBranch = basePackages[0].gitBranch
    let gitCommit = basePackages[0].gitCommit
    let aur = basePackages[0].repo == "aur"
    let branch = gitBranch.get("master")

    let cloneCode = forkWait(() => execResult(gitCmd, "-C", config.tmpRoot,
        "clone", "-q", gitUrl, "-b", branch, "--single-branch", base))

    if cloneCode == 0:
      if gitCommit.isSome:
        let code = forkWait(() => execResult(gitCmd, "-C", repoPath,
          "reset", "-q", "--hard", gitCommit.unsafeGet))
        (code, none(string))
      elif aur: (block:
        let commit = bisectVersion(repoPath, config.debug, none(string),
          "srcinfo", basePackages[0].gitPath.get("."), basePackages[0].version)

        if commit.isSome:
          let code = forkWait(() => execResult(gitCmd, "-C", repoPath,
            "reset", "-q", "--hard", commit.unsafeGet))
          (code, none(string))
        else:
          (1, none(string)))
      else:
        (1, none(string))
    else:
      (cloneCode, none(string))

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
    reference*: PackageReference
    repo*: Option[string]

  SyncPackageTarget* = object of PackageTarget
    foundInfo*: Option[SyncFoundInfo]

  FullPackageTarget*[T] = object of SyncPackageTarget
    pkgInfo*: Option[T]

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
  args.targets.map(target => (block:
    let splitTarget = target.split('/', 2)
    let (repo, nameConstraint) = if splitTarget.len == 2:
        (some(splitTarget[0]), splitTarget[1])
      else:
        (none(string), target)
    let reference = parsePackageReference(nameConstraint, false)
    PackageTarget(reference: reference, repo: repo)))

proc isAurTargetSync*(target: SyncPackageTarget): bool =
  target.foundInfo.isNone and (target.repo.isNone or target.repo == some("aur"))

proc isAurTargetFull*[T: RpcPackageInfo](target: FullPackageTarget[T]): bool =
  target.foundInfo.isSome and target.foundInfo.unsafeGet.repo == "aur"

proc findSyncTargets*(handle: ptr AlpmHandle, dbs: seq[ptr AlpmDatabase],
  targets: seq[PackageTarget], allowGroups: bool, checkProvides: bool):
  (seq[SyncPackageTarget], seq[string]) =
  let dbTable = dbs.map(d => ($d.name, d)).toTable

  proc checkProvided(reference: PackageReference, db: ptr AlpmDatabase): bool =
    for pkg in db.packages:
      for provides in pkg.provides:
        if reference.isProvidedBy(provides.toPackageReference):
          return true
    return false

  proc findSync(target: PackageTarget): Option[SyncFoundInfo] =
    if target.repo.isSome:
      let repo = target.repo.unsafeGet

      if dbTable.hasKey(repo):
        let db = dbTable[repo]
        let pkg = db[target.reference.name]

        if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference):
          let base = if pkg.base == nil: target.reference.name else: $pkg.base
          return some((repo, some((base, $pkg.version, some($pkg.arch)))))
        elif checkProvides and target.reference.checkProvided(db):
          return some((repo, none(SyncFoundPackageInfo)))
        else:
          return none(SyncFoundInfo)
      else:
        return none(SyncFoundInfo)
    else:
      let directResult = dbs
        .map(db => (block:
          let pkg = db[target.reference.name]
          if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference):
            let base = if pkg.base == nil: target.reference.name else: $pkg.base
            some(($db.name, some((base, $pkg.version, some($pkg.arch)))))
          else:
            none(SyncFoundInfo)))
        .filter(i => i.isSome)
        .optFirst
        .flatten

      if directResult.isSome:
        return directResult
      else:
        if allowGroups and target.reference.constraint.isNone:
          let groupRepo = lc[d | (d <- dbs, g <- d.groups,
            $g.name == target.reference.name), ptr AlpmDatabase].optFirst
          if groupRepo.isSome:
            return groupRepo.map(d => ($d.name, none(SyncFoundPackageInfo)))

        if checkProvides:
          for db in dbs:
            if target.reference.checkProvided(db):
              return some(($db.name, none(SyncFoundPackageInfo)))
          return none(SyncFoundInfo)
        else:
          return none(SyncFoundInfo)

  let syncTargets = targets.map(t => SyncPackageTarget(reference: t.reference,
    repo: t.repo, foundInfo: findSync(t)))
  let checkAurNames = syncTargets.filter(isAurTargetSync).map(t => t.reference.name)
  (syncTargets, checkAurNames)

proc mapAurTargets*[T: RpcPackageInfo](targets: seq[SyncPackageTarget],
  pkgInfos: seq[T]): seq[FullPackageTarget[T]] =
  let aurTable = pkgInfos.map(i => (i.name, i)).toTable

  targets.map(proc (target: SyncPackageTarget): FullPackageTarget[T] =
    let res = if target.foundInfo.isNone and aurTable.hasKey(target.reference.name): (block:
        let pkgInfo = aurTable[target.reference.name]
        if target.reference.isProvidedBy(pkgInfo.toPackageReference):
          some((("aur", some((pkgInfo.base, pkgInfo.version, none(string)))), pkgInfo))
        else:
          none((SyncFoundInfo, T)))
      else:
        none((SyncFoundInfo, T))

    if res.isSome:
      let (syncInfo, pkgInfo) = res.get
      FullPackageTarget[T](reference: target.reference, repo: target.repo,
        foundInfo: some(syncInfo), pkgInfo: some(pkgInfo))
    else:
      FullPackageTarget[T](reference: target.reference, repo: target.repo,
        foundInfo: target.foundInfo, pkgInfo: none(T)))

proc queryUnrequired*(handle: ptr AlpmHandle, withOptional: bool, withoutOptional: bool,
  assumeExplicit: HashSet[string]): (HashSet[string], HashSet[string], HashSet[string]) =
  let (explicit, dependsTable, alternatives) = block:
    var explicit = newSeq[string]()
    var dependsTable = initTable[string,
      HashSet[tuple[reference: PackageReference, optional: bool]]]()
    var alternatives = initTable[string, HashSet[PackageReference]]()

    for pkg in handle.local.packages:
      proc fixProvides(reference: PackageReference): PackageReference =
        if reference.constraint.isNone:
          (reference.name, reference.description,
            some((ConstraintOperation.eq, $pkg.version)))
        else:
          reference

      let depends = toSeq(pkg.depends.items)
        .map(d => d.toPackageReference).toSet
      let optional = toSeq(pkg.optional.items)
        .map(d => d.toPackageReference).toSet
      let provides = toSeq(pkg.provides.items)
        .map(d => d.toPackageReference).map(fixProvides).toSet

      if pkg.reason == AlpmReason.explicit:
        explicit &= $pkg.name
      dependsTable.add($pkg.name,
        depends.map(x => (x, false)) + optional.map(x => (x, true)))
      if provides.len > 0:
        alternatives.add($pkg.name, provides)

    (explicit.toSet + assumeExplicit, dependsTable, alternatives)

  let providedBy = lc[(y, x.key) | (x <- alternatives.namedPairs, y <- x.value),
    tuple[reference: PackageReference, name: string]]

  proc findRequired(withOptional: bool, results: HashSet[string],
    check: HashSet[string]): HashSet[string] =
    let full = results + check

    let direct = lc[x.reference | (y <- dependsTable.namedPairs, y.key in check,
      x <- y.value, withOptional or not x.optional), PackageReference]

    let indirect = lc[x.name | (y <- direct, x <- providedBy,
      y.isProvidedBy(x.reference)), string].toSet

    let checkNext = (direct.map(p => p.name).toSet + indirect) - full
    if checkNext.len > 0: findRequired(withOptional, full, checkNext) else: full

  let installed = toSeq(dependsTable.keys).toSet

  proc findOrphans(withOptional: bool): HashSet[string] =
    let required = findRequired(withOptional, initSet[string](), explicit)
    installed - required

  let withOptionalSet = if withOptional: findOrphans(true) else: initSet[string]()
  let withoutOptionalSet = if withoutOptional: findOrphans(false) else: initSet[string]()

  (installed, withOptionalSet, withoutOptionalSet)

proc formatArgument*(target: PackageTarget): string =
  target.repo.map(r => r & "/" & $target.reference).get($target.reference)

proc ensureTmpOrError*(config: Config): Option[string] =
  let tmpRootExists = try:
    let user = initialUser.get(currentUser)
    discard config.tmpRoot.existsOrCreateDir()
    discard chown(config.tmpRoot, (Uid) user.uid, (Gid) user.gid)
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

      dropPrivileges()
      execResult(args)))

  let (workFirstCommit, checkFirst) = if firstCommit.isSome:
      (firstCommit, false)
    else:
      (forkWaitRedirect(() => (block:
        dropPrivileges()
        execResult(gitCmd, "-C", repoPath,
          "rev-list", "--max-parents=0", "@")))
        .output.optLast, true)

  let (realLastThreeCommits, _) = forkWaitRedirect(() => (block:
    dropPrivileges()
    execResult(gitCmd, "-C", repoPath,
      "rev-list", "--max-count=3", "@")))
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
      let foundVersion = forkWaitRedirect(() => (block:
        dropPrivileges()
        execResult(pkgLibDir & "/bisect",
          compareMethod, repoPath & "/" & relativePath, version)))
        .output.optFirst

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

      let commit = forkWaitRedirect(() => (block:
        dropPrivileges()
        execResult(gitCmd, "-C", repoPath,
          "rev-list", "--max-count=1", "refs/bisect/bad")))
        .output.optFirst

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

proc obtainSrcInfo*(path: string): string =
  let (output, code) = forkWaitRedirect(() => (block:
    discard chdir(path)
    dropPrivileges()
    execResult(makePkgCmd, "--printsrcinfo")))

  if code == 0:
    output.foldl(a & b & "\n", "")
  else:
    ""

proc reloadPkgInfos*(config: Config, path: string, pkgInfos: seq[PackageInfo]): seq[PackageInfo] =
  let srcInfo = obtainSrcInfo(path)
  let res = parseSrcInfo(pkgInfos[0].repo, srcInfo, config.arch,
    pkgInfos[0].gitUrl, pkgInfos[0].gitBranch, pkgInfos[0].gitCommit, pkgInfos[0].gitPath)
  if res.len > 0:
    res
  else:
    pkgInfos

proc obtainBuildPkgInfosInternal(config: Config, bases: seq[LookupBaseGroup],
  pacmanTargetNames: seq[string]): (seq[PackageInfo], seq[string]) =
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
          if forkWait(() => (block:
            dropPrivileges()
            execResult(gitCmd, "-C", config.tmpRoot,
              "clone", "-q", git.url, "-b", git.branch,
              "--single-branch", base))) == 0:
            let commit = bisectVersion(repoPath, config.debug, none(string),
              "source", git.path, version)

            if commit.isNone:
              @[]
            else:
              discard forkWait(() => (block:
                dropPrivileges()
                execResult(gitCmd, "-C", repoPath,
                  "checkout", "-q", commit.unsafeGet)))

              let srcInfo = obtainSrcInfo(repoPath & "/" & git.path)
              parseSrcInfo(repo, srcInfo, config.arch,
                git.url, some(git.branch), commit, some(git.path))
                .filter(i => i.version == version)
          else:
            @[]
        finally:
          removeDirQuiet(repoPath)

      let pkgInfos = lc[x | (r <- lookupResults, x <- findCommitAndGetSrcInfo(r.group.base,
        r.group.version, r.group.repo, r.git.get)), PackageInfo]

      let pkgInfosTable = pkgInfos.map(i => (i.name, i)).toTable

      let foundPkgInfos = lc[x | (y <- pacmanTargetNames,
        x <- pkgInfosTable.opt(y)), PackageInfo]
      let messages = pacmanTargetNames
        .filter(n => not pkgInfosTable.hasKey(n))
        .map(n => tr"$#: failed to get package info" % [n])

      discard rmdir(config.tmpRoot)
      (foundPkgInfos, messages)

proc obtainBuildPkgInfos*[T: RpcPackageInfo](config: Config,
  pacmanTargets: seq[FullPackageTarget[T]]): (seq[PackageInfo], seq[string]) =
  let bases = pacmanTargets
    .map(proc (target: FullPackageTarget[T]): LookupBaseGroup =
      let info = target.foundInfo.get
      let pkg = info.pkg.get
      (pkg.base, pkg.version, pkg.arch.get, info.repo))
    .deduplicate

  let pacmanTargetNames = pacmanTargets.map(t => t.reference.name)
  obtainBuildPkgInfosInternal(config, bases, pacmanTargetNames)

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

    let cloneCode = forkWait(() => (block:
      dropPrivileges()
      execResult(gitCmd, "-C", config.tmpRoot,
        "clone", "-q", gitUrl, "-b", branch, "--single-branch", base)))

    if cloneCode == 0:
      if gitCommit.isSome:
        let code = forkWait(() => (block:
          dropPrivileges()
          execResult(gitCmd, "-C", repoPath,
            "reset", "-q", "--hard", gitCommit.unsafeGet)))
        (code, none(string))
      elif aur:
        (0, none(string))
      else:
        (1, none(string))
    else:
      (cloneCode, none(string))

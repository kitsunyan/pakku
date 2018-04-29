import
  future, options, os, osproc, posix, sequtils, sets, strutils, tables,
  args, config, lists, package, pacman, utils,
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
    foundInfos*: seq[SyncFoundInfo]

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
  target.foundInfos.len == 0 and (target.repo.isNone or target.repo == some("aur"))

proc isAurTargetFull*[T: RpcPackageInfo](target: FullPackageTarget[T]): bool =
  target.foundInfos.len > 0 and target.foundInfos[0].repo == "aur"

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

  proc findSync(target: PackageTarget): seq[SyncFoundInfo] =
    if target.repo.isSome:
      let repo = target.repo.unsafeGet

      if dbTable.hasKey(repo):
        let db = dbTable[repo]
        let pkg = db[target.reference.name]

        if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference):
          let base = if pkg.base == nil: target.reference.name else: $pkg.base
          return @[(repo, some((base, $pkg.version, some($pkg.arch))))]
        elif checkProvides and target.reference.checkProvided(db):
          return @[(repo, none(SyncFoundPackageInfo))]
        else:
          return @[]
      else:
        return @[]
    else:
      if allowGroups and target.reference.constraint.isNone:
        let groupRepo = lc[d | (d <- dbs, g <- d.groups,
          $g.name == target.reference.name), ptr AlpmDatabase].optFirst
        if groupRepo.isSome:
          return @[($groupRepo.unsafeGet.name, none(SyncFoundPackageInfo))]

      let directResults = dbs
        .map(db => (block:
          let pkg = db[target.reference.name]
          if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference):
            let base = if pkg.base == nil: target.reference.name else: $pkg.base
            some(($db.name, some((base, $pkg.version, some($pkg.arch)))))
          else:
            none(SyncFoundInfo)))
        .filter(i => i.isSome)
        .map(i => i.unsafeGet)

      if directResults.len > 0:
        return directResults
      elif checkProvides:
        for db in dbs:
          if target.reference.checkProvided(db):
            return @[($db.name, none(SyncFoundPackageInfo))]
        return @[]
      else:
        return @[]

  let syncTargets = targets.map(t => SyncPackageTarget(reference: t.reference,
    repo: t.repo, foundInfos: findSync(t)))
  let checkAurNames = syncTargets.filter(isAurTargetSync).map(t => t.reference.name)
  (syncTargets, checkAurNames)

proc mapAurTargets*[T: RpcPackageInfo](targets: seq[SyncPackageTarget],
  pkgInfos: seq[T]): seq[FullPackageTarget[T]] =
  let aurTable = pkgInfos.map(i => (i.name, i)).toTable

  targets.map(proc (target: SyncPackageTarget): FullPackageTarget[T] =
    let res = if target.foundInfos.len == 0 and aurTable.hasKey(target.reference.name): (block:
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
        foundInfos: @[syncInfo], pkgInfo: some(pkgInfo))
    else:
      FullPackageTarget[T](reference: target.reference, repo: target.repo,
        foundInfos: target.foundInfos, pkgInfo: none(T)))

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

proc `$`*[T: PackageTarget](target: T): string =
  target.repo.map(proc (r: string): string = r & "/" & $target.reference).get($target.reference)

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
  compareMethod: string, gitSubdir: string, version: string): Option[string] =
  template forkExecWithoutOutput(args: varargs[string]): int =
    forkWait(() => (block:
      discard close(0)
      if not debug:
        discard close(1)
        discard close(2)

      if dropPrivileges():
        execResult(args)
      else:
        quit(1)))

  let (workFirstCommit, checkFirst) = if firstCommit.isSome:
      (firstCommit, false)
    else:
      (forkWaitRedirect(() => (block:
        if dropPrivileges():
          execResult(gitCmd, "-C", repoPath,
            "rev-list", "--max-parents=0", "@")
        else:
          quit(1)))
        .output.optLast, true)

  let (realLastThreeCommits, _) = forkWaitRedirect(() => (block:
    if dropPrivileges():
      execResult(gitCmd, "-C", repoPath,
        "rev-list", "--max-count=3", "@")
    else:
      quit(1)))

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
        if dropPrivileges():
          execResult(pkgLibDir & "/bisect",
            compareMethod, repoPath & "/" & gitSubdir, version)
        else:
          quit(1)))
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
        "bisect", "run", pkgLibDir & "/bisect", compareMethod, gitSubdir, version)

      let commit = forkWaitRedirect(() => (block:
        if dropPrivileges():
          execResult(gitCmd, "-C", repoPath,
            "rev-list", "--max-count=1", "refs/bisect/bad")
        else:
          quit(1)))
        .output.optFirst

      discard forkExecWithoutOutput(gitCmd, "-C", repoPath,
        "bisect", "reset")

      if commit.isSome:
        let checkedCommit = commit.map(checkCommit).flatten
        if checkedCommit.isSome:
          checkedCommit
        else:
          # non-incremental git history (e.g. downgrade without epoch change), bisect again
          bisectVersion(repoPath, debug, commit, compareMethod, gitSubdir, version)
      elif checkFirst and workFirstCommit.isSome:
        checkCommit(workFirstCommit.unsafeGet)
      else:
        none(string)

proc obtainSrcInfo*(path: string): string =
  let (output, code) = forkWaitRedirect(() => (block:
    if dropPrivileges() and chdir(path) == 0:
      execResult(makePkgCmd, "--printsrcinfo")
    else:
      quit(1)))

  if code == 0:
    output.foldl(a & b & "\n", "")
  else:
    ""

proc reloadPkgInfos*(config: Config, path: string, pkgInfos: seq[PackageInfo]): seq[PackageInfo] =
  let srcInfo = obtainSrcInfo(path)
  let res = parseSrcInfo(pkgInfos[0].repo, srcInfo, config.arch,
    pkgInfos[0].gitUrl, pkgInfos[0].gitSubdir)
  if res.len > 0:
    res
  else:
    pkgInfos

proc obtainBuildPkgInfosInternal(config: Config, bases: seq[LookupBaseGroup],
  pacmanTargetNames: seq[string], progressCallback: (int, int) -> void):
  (seq[PackageInfo], seq[string], seq[string]) =
  let lookupResults: seq[LookupGitResult] = bases
    .map(b => (b, lookupGitRepo(b.repo, b.base, b.arch)))
  let notFoundRepos = lookupResults.filter(r => r.git.isNone)

  if notFoundRepos.len > 0:
    let messages = notFoundRepos.map(r => tr"$#: repository not found" % [r.group.base])
    (newSeq[PackageInfo](), newSeq[string](), messages)
  else:
    let message = ensureTmpOrError(config)
    if message.isSome:
      (@[], @[], @[message.unsafeGet])
    else:
      proc findCommitAndGetSrcInfo(base: string, version: string,
        repo: string, git: GitRepo): tuple[pkgInfos: seq[PackageInfo], path: Option[string]] =
        let repoPath = repoPath(config.tmpRoot, base)
        removeDirQuiet(repoPath)

        if forkWait(() => (block:
          if dropPrivileges():
            execResult(gitCmd, "-C", config.tmpRoot,
              "clone", "-q", git.url, "-b", git.branch,
              "--single-branch", base)
          else:
            quit(1))) == 0:
          let commit = bisectVersion(repoPath, config.debug, none(string),
            "source", git.path, version)

          if commit.isNone:
            removeDirQuiet(repoPath)
            (newSeq[PackageInfo](), none(string))
          else:
            discard forkWait(() => (block:
              if dropPrivileges():
                execResult(gitCmd, "-C", repoPath,
                  "checkout", "-q", commit.unsafeGet)
              else:
                quit(1)))

            let srcInfo = obtainSrcInfo(repoPath & "/" & git.path)
            let pkgInfos = parseSrcInfo(repo, srcInfo, config.arch,
              git.url, some(git.path))
              .filter(i => i.version == version)
            (pkgInfos, some(repoPath))
        else:
          removeDirQuiet(repoPath)
          (newSeq[PackageInfo](), none(string))

      progressCallback(0, lookupResults.len)
      let (pkgInfosWithPathsReversed, _) = lookupResults.foldl(block:
        let (list, index) = a
        let res = findCommitAndGetSrcInfo(b.group.base, b.group.version,
          b.group.repo, b.git.get) ^& list
        progressCallback(index + 1, lookupResults.len)
        (res, index + 1),
        (list[tuple[pkgInfos: seq[PackageInfo], path: Option[string]]](), 0))

      let pkgInfosWithPaths = pkgInfosWithPathsReversed.reversed
      let pkgInfos = lc[x | (y <- pkgInfosWithPaths, x <- y.pkgInfos), PackageInfo]
      let paths = lc[x | (y <- pkgInfosWithPaths, x <- y.path), string]

      let pkgInfosTable = pkgInfos.map(i => (i.name, i)).toTable

      let foundPkgInfos = lc[x | (y <- pacmanTargetNames,
        x <- pkgInfosTable.opt(y)), PackageInfo]
      let errorMessages = pacmanTargetNames
        .filter(n => not pkgInfosTable.hasKey(n))
        .map(n => tr"$#: failed to get package info" % [n])

      if errorMessages.len > 0:
        for path in paths:
          removeDirQuiet(path)
      discard rmdir(config.tmpRoot)
      (foundPkgInfos, paths, errorMessages)

proc obtainBuildPkgInfos*[T: RpcPackageInfo](config: Config,
  pacmanTargets: seq[FullPackageTarget[T]], progressCallback: (int, int) -> void):
  (seq[PackageInfo], seq[string], seq[string]) =
  let bases = pacmanTargets
    .map(proc (target: FullPackageTarget[T]): LookupBaseGroup =
      let info = target.foundInfos[0]
      let pkg = info.pkg.get
      (pkg.base, pkg.version, pkg.arch.get, info.repo))
    .deduplicate

  let pacmanTargetNames = pacmanTargets.map(t => t.reference.name)
  obtainBuildPkgInfosInternal(config, bases, pacmanTargetNames, progressCallback)

proc cloneAurRepo*(config: Config, base: string, gitUrl: string): (int, Option[string]) =
  let repoPath = repoPath(config.tmpRoot, base)

  let message = ensureTmpOrError(config)
  if message.isSome:
    (1, message)
  elif repoPath.existsDir():
    (0, none(string))
  else:
    let cloneCode = forkWait(() => (block:
      if dropPrivileges():
        execResult(gitCmd, "-C", config.tmpRoot,
          "clone", "-q", gitUrl, "--single-branch", base)
      else:
        quit(1)))

    if cloneCode != 0:
      (cloneCode, some(tr"$#: failed to clone git repository" % [base]))
    else:
      (0, none(string))

proc cloneAurReposWithPackageInfos*(config: Config, rpcInfos: seq[RpcPackageInfo],
  keepRepos: bool, progressCallback: (int, int) -> void):
  (seq[PackageInfo], seq[PackageInfo], seq[string], seq[string]) =
  let bases: seq[tuple[base: string, gitUrl: string]] = rpcInfos
    .map(i => (i.base, i.gitUrl)).deduplicate

  progressCallback(0, bases.len)

  proc cloneNext(index: int, pkgInfos: List[PackageInfo], paths: List[string],
    errors: List[string]): (seq[PackageInfo], seq[string], seq[string]) =
    if index >= bases.len:
      (toSeq(pkgInfos.items), toSeq(paths.items), toSeq(errors.items))
    else:
      let repoPath = repoPath(config.tmpRoot, bases[index].base)
      removeDirQuiet(repoPath)

      let (cloneCode, cloneErrorMessage) = cloneAurRepo(config,
        bases[index].base, bases[index].gitUrl)

      progressCallback(index + 1, bases.len)

      if cloneCode != 0:
        removeDirQuiet(repoPath)
        cloneNext(index + 1, pkgInfos, paths, cloneErrorMessage.map(m => m ^& errors).get(errors))
      else:
        let srcInfos = try:
          readFile(repoPath & "/.SRCINFO")
        except:
          ""

        let addPkgInfos = parseSrcInfo("aur", srcInfos, config.arch,
          bases[index].gitUrl, none(string), rpcInfos)
        if keepRepos:
          cloneNext(index + 1, addPkgInfos ^& pkgInfos, repoPath ^& paths, errors)
        else:
          removeDirQuiet(repoPath)
          cloneNext(index + 1, addPkgInfos ^& pkgInfos, paths, errors)

  let (fullPkgInfos, paths, errors) = cloneNext(0, nil, nil, nil)
  let pkgInfosTable = fullPkgInfos.map(i => (i.name, i)).toTable
  let resultPkgInfos = lc[x | (y <- rpcInfos, x <- pkgInfosTable.opt(y.name)), PackageInfo]

  let names = rpcInfos.map(i => i.name).toSet
  let additionalPkgInfos = fullPkgInfos.filter(i => not (i.name in names))

  discard rmdir(config.tmpRoot)
  (resultPkgInfos, additionalPkgInfos, paths, errors)

import
  future, options, os, osproc, posix, sequtils, sets, strutils, tables,
  args, config, format, lists, package, pacman, utils,
  "wrapper/alpm"

type
  CacheKind* {.pure.} = enum
    repositories, packages

  BareKind* {.pure.} = enum
    pkg, repo

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
    destination*: Option[string]

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

proc checkAndRefreshUpgradeInternal(color: bool, upgrade: bool, args: seq[Argument]):
  tuple[code: int, args: seq[Argument]] =
  let refreshCount = args.count(%%%"refresh")
  let upgradeCount = if upgrade: args.count(%%%"sysupgrade") else: 0
  if refreshCount > 0 or upgradeCount > 0:
    let code = pacmanRun(true, color, args
      .keepOnlyOptions(commonOptions, transactionOptions, upgradeOptions) &
      ("S", none(string), ArgumentType.short) &
      ("y", none(string), ArgumentType.short).repeat(refreshCount) &
      ("u", none(string), ArgumentType.short).repeat(upgradeCount))

    let callArgs = args
      .filter(arg => not arg.matchOption(%%%"refresh") and
        (upgradeCount == 0 or not arg.matchOption(%%%"sysupgrade")))
    (code, callArgs)
  else:
    (0, args)

template checkAndRefreshUpgrade*(color: bool, args: seq[Argument]):
  tuple[code: int, args: seq[Argument]] =
  checkAndRefreshUpgradeInternal(color, true, args)

template checkAndRefresh*(color: bool, args: seq[Argument]):
  tuple[code: int, args: seq[Argument]] =
  checkAndRefreshUpgradeInternal(color, false, args)

proc noconfirm*(args: seq[Argument]): bool =
  args
    .filter(arg => arg.matchOption(%%%"confirm") or
      arg.matchOption(%%%"noconfirm")).optLast
    .map(arg => arg.key == "noconfirm").get(false) or
    args.check(%%%"ask")

proc packageTargets*(args: seq[Argument], parseDestination: bool): seq[PackageTarget] =
  args.targets.map(target => (block:
    let (noDestinationTarget, destination) = if parseDestination: (block:
        let split = target.split("::", 2)
        if split.len == 2:
          (split[0], some(split[1]))
        else:
          (target, none(string)))
      else:
        (target, none(string))

    let splitRepoTarget = noDestinationTarget.split('/', 2)
    let (repo, nameConstraint) = if splitRepoTarget.len == 2:
        (some(splitRepoTarget[0]), splitRepoTarget[1])
      else:
        (none(string), noDestinationTarget)

    let reference = parsePackageReference(nameConstraint, false)
    PackageTarget(reference: reference, repo: repo, destination: destination)))

proc isAurTargetSync*(target: SyncPackageTarget, aurRepo: string): bool =
  target.foundInfos.len == 0 and (target.repo.isNone or target.repo == some(aurRepo))

proc isAurTargetFull*[T: RpcPackageInfo](target: FullPackageTarget[T], aurRepo: string): bool =
  target.foundInfos.len > 0 and target.foundInfos[0].repo == aurRepo

proc filterNotFoundSyncTargetsInternal(syncTargets: seq[SyncPackageTarget],
  pkgInfoReferencesTable: Table[string, PackageReference],
  upToDateNeededTable: Table[string, PackageReference],
  aurRepo: string): seq[SyncPackageTarget] =
  # collect packages which were found neither in sync DB nor in AUR
  syncTargets.filter(t => not (upToDateNeededTable.opt(t.reference.name)
    .map(r => t.reference.isProvidedBy(r, true)).get(false)) and t.foundInfos.len == 0 and
    not (t.isAurTargetSync(aurRepo) and pkgInfoReferencesTable.opt(t.reference.name)
    .map(r => t.reference.isProvidedBy(r, true)).get(false)))

proc filterNotFoundSyncTargets*[T: RpcPackageInfo](syncTargets: seq[SyncPackageTarget],
  pkgInfos: seq[T], upToDateNeededTable: Table[string, PackageReference],
  aurRepo: string): seq[SyncPackageTarget] =
  let pkgInfoReferencesTable = pkgInfos.map(i => (i.name, i.toPackageReference)).toTable
  filterNotFoundSyncTargetsInternal(syncTargets,
    pkgInfoReferencesTable, upToDateNeededTable, aurRepo)

proc printSyncNotFound*(config: Config, notFoundTargets: seq[SyncPackageTarget]) =
  let dbs = config.dbs.toSet

  for target in notFoundTargets:
    if target.repo.isNone or target.repo == some(config.aurRepo) or target.repo.unsafeGet in dbs:
      printError(config.color, trp("target not found: %s\n") % [$target.reference])
    else:
      printError(config.color, trp("database not found: %s\n") % [target.repo.unsafeGet])

proc findSyncTargets*(handle: ptr AlpmHandle, dbs: seq[ptr AlpmDatabase],
  targets: seq[PackageTarget], aurRepo: string, allowGroups: bool, checkProvides: bool):
  (seq[SyncPackageTarget], seq[string]) =
  let dbTable = dbs.map(d => ($d.name, d)).toTable

  proc checkProvided(reference: PackageReference, db: ptr AlpmDatabase): bool =
    for pkg in db.packages:
      for provides in pkg.provides:
        if reference.isProvidedBy(provides.toPackageReference, true):
          return true
    return false

  proc findSync(target: PackageTarget): seq[SyncFoundInfo] =
    if target.repo.isSome:
      let repo = target.repo.unsafeGet

      if dbTable.hasKey(repo):
        let db = dbTable[repo]
        let pkg = db[target.reference.name]

        if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference, true):
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
          if pkg != nil and target.reference.isProvidedBy(pkg.toPackageReference, true):
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
    repo: t.repo, destination: t.destination, foundInfos: findSync(t)))
  let checkAurNames = syncTargets.filter(t => t.isAurTargetSync(aurRepo)).map(t => t.reference.name)
  (syncTargets, checkAurNames)

proc mapAurTargets*[T: RpcPackageInfo](targets: seq[SyncPackageTarget],
  pkgInfos: seq[T], aurRepo: string): seq[FullPackageTarget[T]] =
  let aurTable = pkgInfos.map(i => (i.name, i)).toTable

  targets.map(proc (target: SyncPackageTarget): FullPackageTarget[T] =
    let res = if target.foundInfos.len == 0 and aurTable.hasKey(target.reference.name): (block:
        let pkgInfo = aurTable[target.reference.name]
        if target.reference.isProvidedBy(pkgInfo.toPackageReference, true):
          some(((aurRepo, some((pkgInfo.base, pkgInfo.version, none(string)))), pkgInfo))
        else:
          none((SyncFoundInfo, T)))
      else:
        none((SyncFoundInfo, T))

    if res.isSome:
      let (syncInfo, pkgInfo) = res.get
      FullPackageTarget[T](reference: target.reference, repo: target.repo,
        destination: target.destination, foundInfos: @[syncInfo], pkgInfo: some(pkgInfo))
    else:
      FullPackageTarget[T](reference: target.reference, repo: target.repo,
        destination: target.destination, foundInfos: target.foundInfos, pkgInfo: none(T)))

proc queryUnrequired*(handle: ptr AlpmHandle, withOptional: bool, withoutOptional: bool,
  assumeExplicit: HashSet[string]): (seq[PackageReference], HashSet[string], HashSet[string],
  Table[string, HashSet[PackageReference]]) =
  let (installed, explicit, dependsTable, alternatives) = block:
    var installed = newSeq[PackageReference]()
    var explicit = newSeq[string]()
    var dependsTable = initTable[string,
      HashSet[tuple[reference: PackageReference, optional: bool]]]()
    var alternatives = initTable[string, HashSet[PackageReference]]()

    for pkg in handle.local.packages:
      proc fixProvides(reference: PackageReference): PackageReference =
        if reference.constraint.isNone:
          (reference.name, reference.description,
            some((ConstraintOperation.eq, $pkg.version, true)))
        else:
          reference

      let depends = toSeq(pkg.depends.items)
        .map(d => d.toPackageReference).toSet
      let optional = toSeq(pkg.optional.items)
        .map(d => d.toPackageReference).toSet
      let provides = toSeq(pkg.provides.items)
        .map(d => d.toPackageReference).map(fixProvides).toSet

      installed.add(pkg.toPackageReference)
      if pkg.reason == AlpmReason.explicit:
        explicit &= $pkg.name
      dependsTable.add($pkg.name,
        depends.map(x => (x, false)) + optional.map(x => (x, true)))
      if provides.len > 0:
        alternatives.add($pkg.name, provides)

    (installed, explicit.toSet + assumeExplicit, dependsTable, alternatives)

  let providedBy = lc[(y, x.key) | (x <- alternatives.namedPairs, y <- x.value),
    tuple[reference: PackageReference, name: string]]

  proc findRequired(withOptional: bool, results: HashSet[string],
    check: HashSet[string]): HashSet[string] =
    let full = results + check

    let direct = lc[x.reference | (y <- dependsTable.namedPairs, y.key in check,
      x <- y.value, withOptional or not x.optional), PackageReference]

    let indirect = lc[x.name | (y <- direct, x <- providedBy,
      y.isProvidedBy(x.reference, true)), string].toSet

    let checkNext = (direct.map(p => p.name).toSet + indirect) - full
    if checkNext.len > 0: findRequired(withOptional, full, checkNext) else: full

  let installedNames = installed.map(i => i.name).toSet

  proc findOrphans(withOptional: bool): HashSet[string] =
    let required = findRequired(withOptional, initSet[string](), explicit)
    installedNames - required

  let withOptionalSet = if withOptional: findOrphans(true) else: initSet[string]()
  let withoutOptionalSet = if withoutOptional: findOrphans(false) else: initSet[string]()

  (installed, withOptionalSet, withoutOptionalSet, alternatives)

proc `$`*[T: PackageTarget](target: T): string =
  target.repo.map(proc (r: string): string = r & "/" & $target.reference).get($target.reference)

template tmpRoot(config: Config, dropPrivileges: bool): string =
  if dropPrivileges: config.tmpRootInitial else: config.tmpRootCurrent

template userCache(config: Config, dropPrivileges: bool): string =
  if dropPrivileges: config.userCacheInitial else: config.userCacheCurrent

template cache*(userCache: string, cacheKind: CacheKind): string =
  userCache & "/" & $cacheKind

proc createDirRecursive(dir: string, chownUser: Option[User]): bool =
  let segments = dir.split("/").filter(x => not (x.len == 0 or x == "."))

  proc createDirIndex(index: int): bool =
    if index < segments.len:
      let path = (if dir.len > 0 and dir[0] == '/': "/" else: "") &
        segments[0 .. index].join("/")
      try:
        let exists = path.existsOrCreateDir()
        if chownUser.isSome and (not exists or index == segments.len - 1):
          discard chown(path, (Uid) chownUser.unsafeGet.uid, (Gid) chownUser.unsafeGet.gid)
        createDirIndex(index + 1)
      except:
        false
    else:
      true

  createDirIndex(0)

proc ensureDirOrError(dir: string, dropPrivileges: bool): Option[string] =
  let user = if dropPrivileges: some(initialUser.get(currentUser)) else: none(User)

  if not createDirRecursive(dir, user):
    some(tr"failed to create directory '$#'" % [dir])
  else:
    none(string)

proc ensureTmpOrError*(config: Config, dropPrivileges: bool): Option[string] =
  ensureDirOrError(config.tmpRoot(dropPrivileges), dropPrivileges)

proc ensureUserCacheOrError*(config: Config, cacheKind: CacheKind,
  dropPrivileges: bool): Option[string] =
  ensureDirOrError(config.userCache(dropPrivileges).cache(cacheKind), dropPrivileges)

proc getGitFiles*(repoPath: string, gitSubdir: Option[string],
  dropPrivileges: bool): seq[string] =
  if gitSubdir.isSome:
    forkWaitRedirect(() => (block:
      if not dropPrivileges or dropPrivileges():
        execResult(gitCmd, "-C", repoPath, "ls-tree", "-r", "--name-only", "@",
          gitSubdir.unsafeGet & "/")
      else:
        quit(1)))
      .output
      .map(s => s[gitSubdir.unsafeGet.len + 1 .. ^1])
  else:
    forkWaitRedirect(() => (block:
      if not dropPrivileges or dropPrivileges():
        execResult(gitCmd, "-C", repoPath, "ls-tree", "-r", "--name-only", "@")
      else:
        quit(1)))
      .output

proc bisectVersion(repoPath: string, debug: bool, firstCommit: Option[string],
  compareMethod: string, gitSubdir: string, version: string,
  dropPrivileges: bool): Option[string] =
  template forkExecWithoutOutput(args: varargs[string]): int =
    forkWait(() => (block:
      discard close(0)
      if not debug:
        discard close(1)
        discard close(2)

      if not dropPrivileges or dropPrivileges():
        execResult(args)
      else:
        quit(1)))

  let (workFirstCommit, checkFirst) = if firstCommit.isSome:
      (firstCommit, false)
    else:
      (forkWaitRedirect(() => (block:
        if not dropPrivileges or dropPrivileges():
          execResult(gitCmd, "-C", repoPath,
            "rev-list", "--max-parents=0", "@")
        else:
          quit(1)))
        .output.optLast, true)

  let (realLastThreeCommits, _) = forkWaitRedirect(() => (block:
    if not dropPrivileges or dropPrivileges():
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
        if not dropPrivileges or dropPrivileges():
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
        if not dropPrivileges or dropPrivileges():
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
          bisectVersion(repoPath, debug, commit, compareMethod, gitSubdir,
            version, dropPrivileges)
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

template bareFullName*(bareKind: BareKind, bareName: string): string =
  $bareKind & "-" & bareName & ".git"

proc bareFullNameDeconstruct*(bareKind: BareKind, bareFullName: string): Option[string] =
  if bareFullName.find($bareKind & '-') == 0 and bareFullName.rfind(".git") == bareFullName.len - 4:
    some(bareFullName[($bareKind).len + 1 .. ^5])
  else:
    none(string)

proc cloneBareRepo(config: Config, bareKind: BareKind, bareName: string,
  url: string, branchOption: Option[string], dropPrivileges: bool): Option[string] =
  let fullName = bareFullName(bareKind, bareName)
  let cachePath = config.userCache(dropPrivileges).cache(CacheKind.repositories)
  let repoPath = repoPath(cachePath, fullName)

  if forkWait(() => (block:
    if not dropPrivileges or dropPrivileges():
      if existsDir(repoPath):
        let branch = branchOption.get("master")
        execResult(gitCmd, "-C", repoPath, "fetch", "-q", "--no-tags",
          "origin", branch & ":" & branch)
      else:
        execResult(gitCmd, "-C", cachePath, "clone", "-q", "--bare", "--no-tags",
          url, fullName)
    else:
      quit(1))) == 0:
    some(repoPath)
  else:
    none(string)

proc cloneBareRepos*(config: Config, bareKind: BareKind, gitRepos: seq[GitRepo],
  progressCallback: (int, int) -> void, dropPrivileges: bool): (int, seq[string]) =
  let message = ensureUserCacheOrError(config, CacheKind.repositories, dropPrivileges)
  if message.isSome:
    (0, @[message.unsafeGet])
  else:
    let bare = gitRepos
      .filter(t => t.bareName.isSome)
      .map(r => (r.bareName.unsafeGet, r.url, r.branch))
      .deduplicate

    proc cloneNext(index: int, messages: List[string]): seq[string] =
      progressCallback(index, bare.len)

      if index >= bare.len:
        toSeq(messages.reversed.items)
      else:
        let (bareName, url, branch) = bare[index]
        let repoPath = cloneBareRepo(config, bareKind, bareName, url, branch, dropPrivileges)
        if repoPath.isSome:
          cloneNext(index + 1, messages)
        else:
          let message = tr"$#: failed to clone git repository" % [bareName]
          cloneNext(index + 1, message ^& messages)

    (bare.len, cloneNext(0, nil))

proc clonePackageRepoInternal(config: Config, base: string, version: string,
  git: GitRepo, dropPrivileges: bool): Option[string] =
  let repoPath = repoPath(config.tmpRoot(dropPrivileges), base)
  removeDirQuiet(repoPath)

  let url = if git.bareName.isSome:
      repoPath(config.userCache(dropPrivileges).cache(CacheKind.repositories),
        bareFullName(BareKind.repo, git.bareName.unsafeGet))
    else:
      git.url

  if forkWait(() => (block:
    if not dropPrivileges or dropPrivileges():
      if git.branch.isSome:
        execResult(gitCmd, "-C", config.tmpRoot(dropPrivileges),
          "clone", "-q", url, "-b", git.branch.unsafeGet, "--single-branch", base)
      else:
        execResult(gitCmd, "-C", config.tmpRoot(dropPrivileges),
          "clone", "-q", url, "--single-branch", base)
    else:
      quit(1))) == 0:
    let commit = bisectVersion(repoPath, config.debug, none(string),
      "source", git.path, version, dropPrivileges)

    if commit.isNone:
      removeDirQuiet(repoPath)
      none(string)
    else:
      discard forkWait(() => (block:
        if not dropPrivileges or dropPrivileges():
          execResult(gitCmd, "-C", repoPath,
            "checkout", "-q", commit.unsafeGet)
        else:
          quit(1)))

      some(repoPath)
  else:
    removeDirQuiet(repoPath)
    none(string)

proc clonePackageRepo*(config: Config, base: string, version: string,
  git: GitRepo, dropPrivileges: bool): Option[string] =
  let message = ensureTmpOrError(config, dropPrivileges)
  if message.isSome:
    message
  else:
    let repoPath = clonePackageRepoInternal(config, base, version, git, dropPrivileges)
    if repoPath.isNone:
      some(tr"$#: failed to clone git repository" % [base])
    else:
      none(string)

proc obtainBuildPkgInfosInternal(config: Config, bases: seq[LookupBaseGroup],
  pacmanTargetNames: seq[string], progressCallback: (int, int) -> void, dropPrivileges: bool):
  (seq[PackageInfo], seq[string], seq[string]) =
  let lookupResults: seq[tuple[group: LookupBaseGroup, git: Option[GitRepo]]] = bases
    .map(b => (b, lookupGitRepo(b.repo, b.base, b.arch)))
  let notFoundRepos = lookupResults.filter(r => r.git.isNone)

  if notFoundRepos.len > 0:
    let messages = notFoundRepos.map(r => tr"$#: repository not found" % [r.group.base])
    (newSeq[PackageInfo](), newSeq[string](), messages)
  else:
    let message = ensureTmpOrError(config, dropPrivileges)
    if message.isSome:
      (@[], @[], @[message.unsafeGet])
    else:
      let (bcount, berrors) = cloneBareRepos(config, BareKind.repo,
        lookupResults.map(r => r.git.unsafeGet),
        proc (progress: int, count: int) = progressCallback(progress, count + lookupResults.len),
        dropPrivileges)

      if berrors.len > 0:
        discard rmdir(config.tmpRoot(dropPrivileges))
        (newSeq[PackageInfo](), newSeq[string](), berrors)
      else:
        proc findCommitAndGetSrcInfo(base: string, version: string,
          repo: string, git: GitRepo): tuple[pkgInfos: seq[PackageInfo], path: Option[string]] =
          let repoPath = clonePackageRepoInternal(config, base, version, git, dropPrivileges)

          if repoPath.isSome:
            let srcInfo = obtainSrcInfo(repoPath.unsafeGet & "/" & git.path)
            let pkgInfos = parseSrcInfo(repo, srcInfo, config.arch,
              git.url, some(git.path))
              .filter(i => i.version == version)
            (pkgInfos, repoPath)
          else:
            (newSeq[PackageInfo](), none(string))

        let (pkgInfosWithPathsReversed, _) = lookupResults.foldl(block:
          let (list, index) = a
          let res = findCommitAndGetSrcInfo(b.group.base, b.group.version,
            b.group.repo, b.git.unsafeGet) ^& list
          progressCallback(bcount + index + 1, bcount + lookupResults.len)
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
        discard rmdir(config.tmpRoot(dropPrivileges))
        (foundPkgInfos, paths, errorMessages)

proc obtainBuildPkgInfos*[T: RpcPackageInfo](config: Config,
  pacmanTargets: seq[FullPackageTarget[T]], progressCallback: (int, int) -> void,
  dropPrivileges: bool): (seq[PackageInfo], seq[string], seq[string]) =
  let bases = pacmanTargets
    .map(proc (target: FullPackageTarget[T]): LookupBaseGroup =
      let info = target.foundInfos[0]
      let pkg = info.pkg.get
      (pkg.base, pkg.version, pkg.arch.get, info.repo))
    .deduplicate

  let pacmanTargetNames = pacmanTargets.map(t => t.reference.name)
  obtainBuildPkgInfosInternal(config, bases, pacmanTargetNames, progressCallback, dropPrivileges)

proc cloneAurRepo*(config: Config, base: string, gitUrl: string,
  dropPrivileges: bool): (int, Option[string]) =
  let repoPath = repoPath(config.tmpRoot(dropPrivileges), base)

  let message = block:
    let message = ensureUserCacheOrError(config, CacheKind.repositories, dropPrivileges)
    if message.isNone:
      ensureTmpOrError(config, dropPrivileges)
    else:
      message

  if message.isSome:
    (1, message)
  elif repoPath.existsDir():
    (0, none(string))
  else:
    let fullName = bareFullName(BareKind.pkg, base)
    let cachePath = config.userCache(dropPrivileges).cache(CacheKind.repositories)
    let bareRepoPath = repoPath(cachePath, fullName)

    let cloneBareCode = forkWait(() => (block:
      if not dropPrivileges or dropPrivileges():
        if existsDir(bareRepoPath):
          execResult(gitCmd, "-C", bareRepoPath, "fetch", "-q", "--no-tags",
            "origin", "master:master")
        else:
          execResult(gitCmd, "-C", cachePath, "clone", "-q", "--bare", "--no-tags",
            gitUrl, "--single-branch", fullName)
      else:
        quit(1)))

    let cloneCode = if cloneBareCode == 0:
        forkWait(() => (block:
          if not dropPrivileges or dropPrivileges():
            execResult(gitCmd, "-C", config.tmpRoot(dropPrivileges),
              "clone", "-q", bareRepoPath, "--single-branch", base)
          else:
            quit(1)))
      else:
        cloneBareCode

    if cloneCode != 0:
      (cloneCode, some(tr"$#: failed to clone git repository" % [base]))
    else:
      (0, none(string))

proc cloneAurReposWithPackageInfos*(config: Config, rpcInfos: seq[RpcPackageInfo],
  keepRepos: bool, progressCallback: (int, int) -> void, dropPrivileges: bool):
  (seq[PackageInfo], seq[PackageInfo], seq[string], seq[string]) =
  let bases: seq[tuple[base: string, gitUrl: string]] = rpcInfos
    .map(i => (i.base, i.gitUrl)).deduplicate

  progressCallback(0, bases.len)

  proc cloneNext(index: int, pkgInfos: List[PackageInfo], paths: List[string],
    errors: List[string]): (seq[PackageInfo], seq[string], seq[string]) =
    if index >= bases.len:
      (toSeq(pkgInfos.items), toSeq(paths.items), toSeq(errors.items))
    else:
      let repoPath = repoPath(config.tmpRoot(dropPrivileges), bases[index].base)
      removeDirQuiet(repoPath)

      let (cloneCode, cloneErrorMessage) = cloneAurRepo(config,
        bases[index].base, bases[index].gitUrl, dropPrivileges)

      progressCallback(index + 1, bases.len)

      if cloneCode != 0:
        removeDirQuiet(repoPath)
        cloneNext(index + 1, pkgInfos, paths, cloneErrorMessage.map(m => m ^& errors).get(errors))
      else:
        let srcInfos = try:
          readFile(repoPath & "/.SRCINFO")
        except:
          ""

        let addPkgInfos = parseSrcInfo(config.aurRepo, srcInfos, config.arch,
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

  discard rmdir(config.tmpRoot(dropPrivileges))
  (resultPkgInfos, additionalPkgInfos, paths, errors)

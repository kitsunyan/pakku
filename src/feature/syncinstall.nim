import
  algorithm, future, options, os, posix, sequtils, sets, strutils, tables,
  "../args", "../aur", "../config", "../common", "../format", "../lists", "../package",
    "../pacman", "../utils",
  "../wrapper/alpm"

type
  Installed = tuple[
    name: string,
    version: string,
    groups: seq[string],
    explicit: bool
  ]

  SatisfyResult = tuple[
    installed: bool,
    name: string,
    buildPkgInfo: Option[PackageInfo]
  ]

  LocalIsNewer = tuple[
    name: string,
    version: string,
    aurVersion: string
  ]

  ReplacePkgInfo = tuple[
    name: Option[string],
    pkgInfo: PackageInfo
  ]

  BuildResult = tuple[
    ext: string,
    replacePkgInfos: seq[ReplacePkgInfo]
  ]

proc groupsSeq(pkg: ptr AlpmPackage): seq[string] =
  toSeq(pkg.groups.items).map(s => $s)

proc orderInstallation(ordered: seq[seq[seq[PackageInfo]]], grouped: seq[seq[PackageInfo]],
  satisfied: Table[PackageReference, SatisfyResult]): seq[seq[seq[PackageInfo]]] =
  let orderedNamesSet = lc[c.name | (a <- ordered, b <- a, c <- b), string].toSet

  proc hasBuildDependency(pkgInfos: seq[PackageInfo]): bool =
    for pkgInfo in pkgInfos:
      for reference in pkgInfo.allDepends:
        for satres in satisfied.opt(reference):
          if satres.buildPkgInfo.isSome and
            not (satres.buildPkgInfo.unsafeGet in pkgInfos) and
            not (satres.buildPkgInfo.unsafeGet.name in orderedNamesSet):
            return true
    return false

  let split: seq[tuple[pkgInfos: seq[PackageInfo], dependent: bool]] =
    grouped.map(i => (i, i.hasBuildDependency))

  let newOrdered = ordered & split.filter(s => not s.dependent).map(s => s.pkgInfos)
  let unordered = split.filter(s => s.dependent).map(s => s.pkgInfos)

  if unordered.len > 0:
    if unordered.len == grouped.len:
      newOrdered & unordered
    else:
      orderInstallation(newOrdered, unordered, satisfied)
  else:
    newOrdered

proc orderInstallation(pkgInfos: seq[PackageInfo],
  satisfied: Table[PackageReference, SatisfyResult]): seq[seq[seq[PackageInfo]]] =
  let grouped = pkgInfos.groupBy(i => i.base).map(p => p.values)

  orderInstallation(@[], grouped, satisfied)
    .map(x => x.filter(s => s.len > 0))
    .filter(x => x.len > 0)

proc findDependencies(config: Config, handle: ptr AlpmHandle, dbs: seq[ptr AlpmDatabase],
  satisfied: Table[PackageReference, SatisfyResult], unsatisfied: seq[PackageReference],
  totalAurFail: seq[PackageReference], printMode: bool, noaur: bool):
  (Table[PackageReference, SatisfyResult], seq[PackageReference]) =
  proc checkDependencyCycle(pkgInfo: PackageInfo, reference: PackageReference): bool =
    for checkReference in pkgInfo.allDepends:
      if checkReference == reference:
        return false
      let buildPkgInfo = satisfied.opt(checkReference)
        .map(r => r.buildPkgInfo).flatten
      if buildPkgInfo.isSome and not checkDependencyCycle(buildPkgInfo.unsafeGet, reference):
        return false
    return true

  proc findInSatisfied(reference: PackageReference): Option[PackageInfo] =
    for satref, res in satisfied.pairs:
      if res.buildPkgInfo.isSome:
        let pkgInfo = res.buildPkgInfo.unsafeGet
        if satref == reference or reference.isProvidedBy(pkgInfo.toPackageReference):
          return some(pkgInfo)
        for provides in pkgInfo.provides:
          if reference.isProvidedBy(provides) and
            checkDependencyCycle(pkgInfo, reference):
            return some(pkgInfo)
    return none(PackageInfo)

  proc findInDatabaseWithGroups(db: ptr AlpmDatabase, reference: PackageReference,
    directName: bool): Option[tuple[name: string, groups: seq[string]]] =
    for pkg in db.packages:
      if reference.isProvidedBy(pkg.toPackageReference):
        return some(($pkg.name, pkg.groupsSeq))
      for provides in pkg.provides:
        if reference.isProvidedBy(provides.toPackageReference):
          if directName:
            return some(($pkg.name, pkg.groupsSeq))
          else:
            return some(($provides.name, pkg.groupsSeq))
    return none((string, seq[string]))

  proc findInDatabase(db: ptr AlpmDatabase, reference: PackageReference,
    directName: bool, checkIgnored: bool): Option[string] =
    let res = findInDatabaseWithGroups(db, reference, directName)
    if res.isSome:
      let r = res.unsafeGet
      if checkIgnored and config.ignored(r.name, r.groups):
        none(string)
      else:
        some(r.name)
    else:
      none(string)

  proc findInDatabases(reference: PackageReference,
    directName: bool, checkIgnored: bool): Option[string] =
    for db in dbs:
      let name = findInDatabase(db, reference, directName, checkIgnored)
      if name.isSome:
        return name
    return none(string)

  proc find(reference: PackageReference): Option[SatisfyResult] =
    let localName = findInDatabase(handle.local, reference, true, false)
    if localName.isSome:
      some((true, localName.unsafeGet, none(PackageInfo)))
    else:
      let pkgInfo = findInSatisfied(reference)
      if pkgInfo.isSome:
        some((false, pkgInfo.unsafeGet.name, pkgInfo))
      else:
        let syncName = findInDatabases(reference, false, true)
        if syncName.isSome:
          some((false, syncName.unsafeGet, none(PackageInfo)))
        else:
          none(SatisfyResult)

  type ReferenceResult = tuple[reference: PackageReference, result: Option[SatisfyResult]]

  let findResult: seq[ReferenceResult] = unsatisfied.map(r => (r, r.find))
  let success = findResult.filter(r => r.result.isSome)
  let aurCheck = findResult.filter(r => r.result.isNone).map(r => r.reference)
    .filter(r => not (r in totalAurFail))

  let (aurSuccess, aurFail) = if not noaur and aurCheck.len > 0: (block:
      let (update, terminate) = if aurCheck.len >= 4 and not printMode:
          printProgressShare(config.progressBar, tr"checking build dependencies")
        else:
          (proc (a: int, b: int) {.closure.} = discard, proc {.closure.} = discard)
      try:
        withAur():
          let (pkgInfos, aerrors) = getAurPackageInfo(aurCheck.map(r => r.name),
            none(seq[RpcPackageInfo]), config.arch, update)
          for e in aerrors: printError(config.color, e)

          let acceptedPkgInfos = pkgInfos.filter(i => not config.ignored(i.name, i.groups))
          let aurTable = acceptedPkgInfos.map(i => (i.name, i)).toTable
          let aurResult = aurCheck.map(proc (reference: PackageReference): ReferenceResult =
            if aurTable.hasKey(reference.name):
              (reference, some((false, reference.name, some(aurTable[reference.name]))))
            else:
              (reference, none(SatisfyResult)))

          let aurSuccess = aurResult.filter(r => r.result.isSome)
          let aurFail = aurResult.filter(r => r.result.isNone).map(r => r.reference)
          (aurSuccess, aurFail)
      finally:
        terminate())
    else:
      (@[], aurCheck)

  let newSatisfied = (toSeq(satisfied.pairs) &
    success.map(r => (r.reference, r.result.unsafeGet)) &
    aurSuccess.map(r => (r.reference, r.result.unsafeGet))).toTable

  let newUnsatisfied = lc[x | (y <- aurSuccess, r <- y.result, i <- r.buildPkgInfo,
    x <- i.allDepends), PackageReference].deduplicate

  let newTotalAurFail = (totalAurFail & aurFail).deduplicate
  let newTotalUnsatisfied = (newUnsatisfied & newTotalAurFail).deduplicate

  if newUnsatisfied.len > 0:
    findDependencies(config, handle, dbs, newSatisfied, newTotalUnsatisfied, newTotalAurFail,
      printMode, noaur)
  else:
    let finallyUnsatisfied = newTotalAurFail.filter(r => not newSatisfied.hasKey(r))
    (newSatisfied, finallyUnsatisfied)

proc findDependencies(config: Config, handle: ptr AlpmHandle,
  dbs: seq[ptr AlpmDatabase], pkgInfos: seq[PackageInfo], printMode: bool, noaur: bool):
  (Table[PackageReference, SatisfyResult], seq[PackageReference]) =
  let satisfied = pkgInfos.map(p => ((p.name, none(string), none(VersionConstraint)),
    (false, p.name, some(p)))).toTable
  let unsatisfied = lc[x | (i <- pkgInfos, x <- i.allDepends), PackageReference].deduplicate
  findDependencies(config, handle, dbs, satisfied, unsatisfied, @[], printMode, noaur)

proc filterNotFoundSyncTargetsInternal(syncTargets: seq[SyncPackageTarget],
  pkgInfoReferencesTable: Table[string, PackageReference],
  upToDateNeededTable: Table[string, PackageReference]): seq[SyncPackageTarget] =
  # collect packages which were found neither in sync DB nor in AUR
  syncTargets.filter(t => not (upToDateNeededTable.opt(t.reference.name)
    .map(r => t.reference.isProvidedBy(r)).get(false)) and t.foundInfos.len == 0 and
    not (t.isAurTargetSync and pkgInfoReferencesTable.opt(t.reference.name)
    .map(r => t.reference.isProvidedBy(r)).get(false)))

proc filterNotFoundSyncTargets[T: RpcPackageInfo](syncTargets: seq[SyncPackageTarget],
  pkgInfos: seq[T], upToDateNeededTable: Table[string, PackageReference]): seq[SyncPackageTarget] =
  let pkgInfoReferencesTable = pkgInfos.map(i => (i.name, i.toPackageReference)).toTable
  filterNotFoundSyncTargetsInternal(syncTargets, pkgInfoReferencesTable, upToDateNeededTable)

proc printSyncNotFound(config: Config, notFoundTargets: seq[SyncPackageTarget]) =
  let dbs = config.dbs.toSet

  for target in notFoundTargets:
    if target.repo.isNone or target.repo == some("aur") or target.repo.unsafeGet in dbs:
      printError(config.color, trp("target not found: %s\n") % [$target.reference])
    else:
      printError(config.color, trp("database not found: %s\n") % [target.repo.unsafeGet])

proc printUnsatisfied(config: Config,
  satisfied: Table[PackageReference, SatisfyResult], unsatisfied: seq[PackageReference]) =
  if unsatisfied.len > 0:
    for _, satres in satisfied.pairs:
      for pkgInfo in satres.buildPkgInfo:
        for reference in pkgInfo.allDepends:
          if reference in unsatisfied:
            printError(config.color,
              trp("unable to satisfy dependency '%s' required by %s\n") %
              [$reference, pkgInfo.name])

proc editLoop(config: Config, base: string, repoPath: string, gitPath: Option[string],
  defaultYes: bool, noconfirm: bool): char =
  proc editFileLoop(file: string): char =
    let default = if defaultYes: 'y' else: 'n'
    let res = printColonUserChoice(config.color,
      tr"View and edit $#?" % [base & "/" & file], ['y', 'n', 's', 'a', '?'],
      default, '?', noconfirm, 'n')

    if res == '?':
      printUserInputHelp(('s', tr"skip all files"),
        ('a', tr"abort operation"))
      editFileLoop(file)
    elif res == 'y':
      let visualEnv = getEnv("VISUAL")
      let editorEnv = getEnv("EDITOR")
      let editor = if visualEnv.len > 0:
          visualEnv
        elif editorEnv.len > 0:
          editorEnv
        else:
          printColonUserInput(config.color, tr"Enter editor executable name" & ":",
            noconfirm, "", "")

      if editor.strip.len == 0:
        'n'
      else:
        discard forkWait(proc: int =
          discard chdir(buildPath(repoPath, gitPath))
          dropPrivileges()
          execResult(bashCmd, "-c", """$1 "$2"""", "bash", editor, file))
        editFileLoop(file)
    else:
      res

  let rawFiles = if gitPath.isSome:
      forkWaitRedirect(() => (block:
        dropPrivileges()
        execResult(gitCmd, "-C", repoPath, "ls-tree", "-r", "--name-only", "@",
          gitPath.unsafeGet & "/")))
        .output
        .map(s => s[gitPath.unsafeGet.len + 1 .. ^1])
    else:
      forkWaitRedirect(() => (block:
        dropPrivileges()
        execResult(gitCmd, "-C", repoPath, "ls-tree", "-r", "--name-only", "@")))
        .output

  let files = ("PKGBUILD" & rawFiles.filter(x => x != ".SRCINFO")).deduplicate

  proc editFileLoopAll(index: int): char =
    if index < files.len:
      let res = editFileLoop(files[index])
      if res == 'n': editFileLoopAll(index + 1) else: res
    else:
      'n'

  editFileLoopAll(0)

proc buildLoop(config: Config, pkgInfos: seq[PackageInfo], noconfirm: bool,
  noextract: bool): (Option[BuildResult], int, bool) =
  let base = pkgInfos[0].base
  let repoPath = repoPath(config.tmpRoot, base)
  let gitPath = pkgInfos[0].gitPath
  let buildPath = buildPath(repoPath, gitPath)

  let confFileEnv = getEnv("MAKEPKG_CONF")
  let confFile = if confFileEnv.len == 0:
      sysConfDir & "/makepkg.conf"
    else:
      confFileEnv

  let workConfFile = config.tmpRoot & "/makepkg.conf"

  let workConfFileCopySuccess = try:
    copyFile(confFile, workConfFile)
    var file: File
    if file.open(workConfFile, fmAppend):
      try:
        file.writeLine("")
        file.writeLine('#'.repeat(73))
        file.writeLine("# PAKKU OVERRIDES")
        file.writeLine('#'.repeat(73))
        file.writeLine("CARCH=" & config.arch.bashEscape)
        file.writeLine("PKGDEST=" & config.tmpRoot.bashEscape)
      finally:
        file.close()
    true
  except:
    discard unlink(workConfFile)
    false

  if not workConfFileCopySuccess:
    printError(config.color, tr"failed to copy config file '$#'" % [confFile])
    (none(BuildResult), 1, false)
  else:
    let envExt = getEnv("PKGEXT")
    let confExt = if envExt.len == 0:
        forkWaitRedirect(() => (block:
          dropPrivileges()
          execResult(bashCmd, "-c",
            "source \"$@\" && echo \"$PKGEXT\"",
            "bash", workConfFile)))
          .output.optFirst.get("")
      else:
        envExt

    let (buildCode, interrupted) = catchInterrupt():
      forkWait(proc: int =
        if chdir(buildPath) == 0:
          discard cunsetenv("MAKEPKG_CONF")
          dropPrivileges()

          if not noextract:
            removeDirQuiet(buildPath & "src")

          let optional: seq[tuple[arg: string, cond: bool]] = @[
            ("-e", noextract),
            ("-m", not config.color)
          ]

          execResult(@[makepkgCmd, "--config", workConfFile, "-f"] &
            optional.filter(o => o.cond).map(o => o.arg))
        else:
          quit(1))

    discard unlink(workConfFile)

    if interrupted:
      (none(BuildResult), buildCode, interrupted)
    elif buildCode != 0:
      printError(config.color, tr"failed to build '$#'" % [base])
      (none(BuildResult), buildCode, false)
    else:
      let resultPkgInfos = reloadPkgInfos(config, buildPath, pkgInfos)

      type ResultInfo = tuple[name: string, baseIndex: int, pkgInfo: Option[PackageInfo]]

      let resultPkgInfosTable = resultPkgInfos.map(i => (i.name, i)).toTable
      let resultByNames: seq[ResultInfo] = pkgInfos
        .map(i => (i.name, i.baseIndex, resultPkgInfosTable.opt(i.name)))

      let resultByIndices: seq[ResultInfo] = if pkgInfos[0].baseCount == resultPkgInfos.len:
          resultByNames.map(res => (block:
            if res.pkgInfo.isNone:
              (res.name, res.baseIndex, some(resultPkgInfos[res.baseIndex]))
            else:
              res))
        else:
          resultByNames

      let failedNames = lc[x.name | (x <- resultByIndices, x.pkgInfo.isNone), string]

      if failedNames.len > 0:
        for name in failedNames:
          printError(config.color, tr"$#: failed to extract package info" % [name])
        (none(BuildResult), 1, false)
      else:
        let targetPkgInfos: seq[ReplacePkgInfo] = resultByIndices
          .map(i => (some(i.name), i.pkgInfo.get))
        let filterNames = targetPkgInfos.map(r => r.pkgInfo.name).toSet
        let additionalPkgInfos: seq[ReplacePkgInfo] = resultPkgInfos
          .filter(i => not (i.name in filterNames))
          .map(i => (none(string), i))
        (some(($confExt, targetPkgInfos & additionalPkgInfos)), 0, false)

proc buildFromSources(config: Config, commonArgs: seq[Argument],
  pkgInfos: seq[PackageInfo], noconfirm: bool): (Option[BuildResult], int) =
  let base = pkgInfos[0].base
  let repoPath = repoPath(config.tmpRoot, base)
  let gitPath = pkgInfos[0].gitPath
  let (cloneCode, cloneErrorMessage) = cloneRepo(config, pkgInfos)

  if cloneCode != 0:
    for e in cloneErrorMessage: printError(config.color, e)
    printError(config.color, tr"$#: failed to clone git repository" % [base])
    (none(BuildResult), cloneCode)
  else:
    proc loop(noextract: bool, showEditLoop: bool): (Option[BuildResult], int) =
      let res = if showEditLoop and not noconfirm:
          editLoop(config, base, repoPath, gitPath, false, noconfirm)
        else:
          'n'

      if res == 'a':
        (none(BuildResult), 1)
      else:
        let (buildResult, code, interrupted) = buildLoop(config, pkgInfos,
          noconfirm, noextract)

        if interrupted:
          (buildResult, 1)
        elif code != 0:
          proc ask(): char =
            let res = printColonUserChoice(config.color,
              tr"Build failed, retry?", ['y', 'e', 'n', '?'], 'n', '?',
              noconfirm, 'n')
            if res == '?':
              printUserInputHelp(('e', tr"retry with --noextract option"))
              ask()
            else:
              res

          let res = ask()
          if res == 'e':
            loop(true, true)
          elif res == 'y':
            loop(false, true)
          else:
            (buildResult, code)
        else:
          (buildResult, code)

    let preBuildCode = if config.preBuildCommand.isSome: (block:
        printColon(config.color, tr"Running pre-build command...")

        let code = forkWait(() => (block:
          discard chdir(buildPath(repoPath, gitPath))
          dropPrivileges()
          execResult(bashCmd, "-c", config.preBuildCommand.unsafeGet)))

        if code != 0 and printColonUserChoice(config.color,
          tr"Command failed, continue?", ['y', 'n'], 'n', 'n',
          noconfirm, 'n') == 'y':
          0
        else:
          code)
      else:
        0

    if preBuildCode != 0:
      (none(BuildResult), preBuildCode)
    else:
      loop(false, false)

proc installGroupFromSources(config: Config, commonArgs: seq[Argument],
  basePackages: seq[seq[PackageInfo]], explicits: HashSet[string],
  noconfirm: bool): (seq[(string, string)], int) =
  proc buildNext(index: int, buildResults: List[BuildResult]): (List[BuildResult], int) =
    if index < basePackages.len:
      let (buildResult, code) = buildFromSources(config, commonArgs,
        basePackages[index], noconfirm)

      if code != 0:
        (buildResults.reversed, code)
      else:
        buildNext(index + 1, buildResult.unsafeGet ^& buildResults)
    else:
      (buildResults.reversed, 0)

  let (buildResults, buildCode) = buildNext(0, nil)

  proc formatArchiveFile(pkgInfo: PackageInfo, ext: string): string =
    let arch = if config.arch in pkgInfo.archs: config.arch else: "any"
    config.tmpRoot & "/" & pkgInfo.name & "-" & pkgInfo.version & "-" & arch & ext

  let allFiles = lc[(r.name, formatArchiveFile(r.pkgInfo, br.ext)) |
    (br <- buildResults, r <- br.replacePkgInfos), tuple[name: Option[string], file: string]]
  let filesTable = allFiles.filter(f => f.name.isSome).map(f => (f.name.unsafeGet, f.file)).toTable
  let install = lc[(i.name, x) | (g <- basePackages, i <- g, x <- filesTable.opt(i.name)),
    tuple[name: string, file: string]]

  proc handleTmpRoot(clear: bool) =
    let installFiles = install.map(p => p.file)
    for pair in allFiles:
      if clear or not (pair.file in installFiles):
        try:
          removeFile(pair.file)
        except:
          discard

    if not clear:
      printWarning(config.color, tr"packages are saved to '$#'" % [config.tmpRoot])

  if buildCode != 0:
    handleTmpRoot(true)
    (newSeq[(string, string)](), buildCode)
  else:
    if currentUser.uid != 0 and printColonUserChoice(config.color,
      tr"Continue installing?", ['y', 'n'], 'y', 'n',
      noconfirm, 'y') != 'y':
      handleTmpRoot(false)
      (newSeq[(string, string)](), 1)
    else:
      let asdeps = install.filter(p => not (p.name in explicits)).map(p => p.file)
      let asexplicit = install.filter(p => p.name in explicits).map(p => p.file)

      proc doInstall(files: seq[string], addArgs: seq[Argument]): int =
        if files.len > 0:
          pacmanRun(true, config.color, commonArgs &
            ("U", none(string), ArgumentType.short) & addArgs &
            files.map(f => (f, none(string), ArgumentType.target)))
        else:
          0

      let asdepsCode = doInstall(asdeps,
        @[("asdeps", none(string), ArgumentType.long)])
      if asdepsCode != 0:
        handleTmpRoot(false)
        (newSeq[(string, string)](), asdepsCode)
      else:
        let asexplicitCode = doInstall(asexplicit,
          @[("asexplicit", none(string), ArgumentType.long)])
        if asexplicitCode != 0:
          handleTmpRoot(false)
          (newSeq[(string, string)](), asexplicitCode)
        else:
          handleTmpRoot(true)
          let installedAs = lc[(r.name.unsafeGet, r.pkgInfo.name) | (br <- buildResults,
            r <- br.replacePkgInfos, r.name.isSome), (string, string)]
          (installedAs, 0)

proc handleInstall(args: seq[Argument], config: Config, upgradeCount: int,
  noconfirm: bool, explicits: HashSet[string], installed: seq[Installed],
  satisfied: Table[PackageReference, SatisfyResult], unsatisfied: seq[PackageReference],
  keepNames: HashSet[string], build: bool, directPacmanTargets: seq[string],
  additionalPacmanTargets: seq[string], basePackages: seq[seq[seq[PackageInfo]]]): int =
  let workDirectPacmanTargets = if build: @[] else: directPacmanTargets

  let (directCode, directSome) = if workDirectPacmanTargets.len > 0 or upgradeCount > 0:
      (pacmanRun(true, config.color, args.filter(arg => not arg.isTarget) &
        workDirectPacmanTargets.map(t => (t, none(string), ArgumentType.target))), true)
    else:
      (0, false)

  let directSatisfiedCode = if directCode == 0 and unsatisfied.len > 0: (block:
      printUnsatisfied(config, satisfied, unsatisfied)
      1)
    else:
      directCode

  if directSatisfiedCode != 0:
    directSatisfiedCode
  else:
    let commonArgs = args.keepOnlyOptions(commonOptions, upgradeCommonOptions)

    let (paths, confirmAndCloneCode) = if basePackages.len > 0: (block:
        let installedVersions = installed.map(i => (i.name, i.version)).toTable

        printPackages(config.color, config.verbosePkgList,
          lc[(i.name, i.repo, installedVersions.opt(i.name), i.version) |
            (g <- basePackages, b <- g, i <- b), PackageInstallFormat]
            .sorted((a, b) => cmp(a.name, b.name)))
        let input = printColonUserChoice(config.color,
          tr"Proceed with building?", ['y', 'n'], 'y', 'n', noconfirm, 'y')

        if input == 'y':
          let (update, terminate) = if config.debug:
              (proc (a: int, b: int) {.closure.} = discard, proc {.closure.} = discard)
            else:
              printProgressShare(config.progressBar, tr"cloning repositories")

          let flatBasePackages = lc[x | (a <- basePackages, x <- a), seq[PackageInfo]]
          update(0, flatBasePackages.len)

          proc cloneNext(index: int, paths: List[string]): (List[string], int) =
            if index < flatBasePackages.len:
              let pkgInfos = flatBasePackages[index]
              let base = pkgInfos[0].base
              let repoPath = repoPath(config.tmpRoot, base)
              let (cloneCode, cloneErrorMessage) = cloneRepo(config, flatBasePackages[index])

              if cloneCode == 0:
                update(index + 1, flatBasePackages.len)
                cloneNext(index + 1, repoPath ^& paths)
              else:
                terminate()
                for e in cloneErrorMessage: printError(config.color, e)
                printError(config.color, tr"$#: failed to clone git repository" %
                  [pkgInfos[0].base])
                ((repoPath ^& paths).reversed, cloneCode)
            else:
              terminate()
              (paths.reversed, 0)

          let (paths, cloneCode) = cloneNext(0, nil)
          if cloneCode != 0:
            (paths, cloneCode)
          else:
            proc checkNext(index: int, skipEdit: bool, skipKeys: bool): int =
              if index < flatBasePackages.len:
                let pkgInfos = flatBasePackages[index]
                let base = pkgInfos[0].base
                let repoPath = repoPath(config.tmpRoot, base)

                let aur = pkgInfos[0].repo == "aur"

                if not skipEdit and aur and not noconfirm and config.aurComments:
                  echo(tr"downloading comments from AUR...")
                  let (comments, error) = downloadAurComments(base)
                  for e in error: printError(config.color, e)
                  if comments.len > 0:
                    let commentsReversed = toSeq(comments.reversed)
                    printComments(config.color, pkgInfos[0].maintainer, commentsReversed)

                let editRes = if skipEdit or noconfirm:
                    'n'
                  else: (block:
                    let defaultYes = aur and not config.viewNoDefault
                    editLoop(config, base, repoPath, pkgInfos[0].gitPath, defaultYes, noconfirm))

                if editRes == 'a':
                  1
                else:
                  let resultPkgInfos = reloadPkgInfos(config,
                    repoPath & "/" & pkgInfos[0].gitPath.get("."), pkgInfos)
                  let pgpKeys = lc[x | (p <- resultPkgInfos, x <- p.pgpKeys), string].deduplicate

                  proc keysLoop(index: int, skipKeys: bool): char =
                    if index >= pgpKeys.len:
                      'n'
                    elif forkWait(() => (block:
                      discard close(0)
                      discard open("/dev/null")
                      discard close(1)
                      discard open("/dev/null")
                      discard close(2)
                      discard open("/dev/null")
                      dropPrivileges()
                      execResult(gpgCmd, "--list-keys", pgpKeys[index]))) == 0:
                      keysLoop(index + 1, skipKeys)
                    else:
                      let res = if skipKeys:
                          'y'
                        else:
                          printColonUserChoice(config.color,
                            tr"Import PGP key $#?" % [pgpKeys[index]], ['y', 'n', 'c', 'a', '?'],
                            'y', '?', noconfirm, 'y')

                      let newSkipKeys = skipKeys or res == 'c'

                      if res == '?':
                        printUserInputHelp(('c', tr"import all keys"),
                          ('a', tr"abort operation"))
                        keysLoop(index, newSkipKeys)
                      elif res == 'y' or newSkipKeys:
                        let importCode = forkWait(() => (block:
                          dropPrivileges()
                          if config.pgpKeyserver.isSome:
                            forkWait(() => execResult(gpgCmd,
                              "--keyserver", config.pgpKeyserver.unsafeGet,
                              "--recv-keys", pgpKeys[index]))
                          else:
                            forkWait(() => execResult(gpgCmd,
                              "--recv-keys", pgpKeys[index]))))

                        if importCode == 0 or newSkipKeys or noconfirm:
                          keysLoop(index + 1, newSkipKeys)
                        else:
                          keysLoop(index, newSkipKeys)
                      elif res == 'n':
                        keysLoop(index + 1, newSkipKeys)
                      else:
                        res

                  let keysRes = keysLoop(0, skipKeys)
                  if keysRes == 'a':
                    1
                  else:
                    checkNext(index + 1, skipEdit or editRes == 's', skipKeys or keysRes == 's')
              else:
                0

            (paths, checkNext(0, false, false))
        else:
          (nil, 1))
      else:
        (nil, 0)

    proc removeTmp() =
      for path in paths:
        removeDirQuiet(path)
      discard rmdir(config.tmpRoot)

    if confirmAndCloneCode != 0:
      removeTmp()
      confirmAndCloneCode
    else:
      let (_, initialUnrequired, initialUnrequiredWithoutOptional) = withAlpm(config.root,
        config.db, newSeq[string](), config.arch, handle, dbs, errors):
        queryUnrequired(handle, true, true, keepNames)

      let (additionalCode, additionalSome) = if additionalPacmanTargets.len > 0: (block:
          printColon(config.color, tr"Installing build dependencies...")

          (pacmanRun(true, config.color, commonArgs &
            ("S", none(string), ArgumentType.short) &
            ("needed", none(string), ArgumentType.long) &
            ("asdeps", none(string), ArgumentType.long) &
            additionalPacmanTargets.map(t => (t, none(string), ArgumentType.target))), true))
        else:
          (0, false)

      if additionalCode != 0:
        removeTmp()
        additionalCode
      else:
        if basePackages.len > 0:
          # check all pacman dependencies were installed
          let unsatisfied = withAlpm(config.root, config.db,
            config.dbs, config.arch, handle, dbs, errors):
            for e in errors: printError(config.color, e)

            proc checkSatisfied(reference: PackageReference): bool =
              for pkg in handle.local.packages:
                if reference.isProvidedBy(pkg.toPackageReference):
                  return true
                for provides in pkg.provides:
                  if reference.isProvidedBy(provides.toPackageReference):
                    return true
              return false

            lc[x.key | (x <- satisfied.namedPairs, not x.value.installed and
              x.value.buildPkgInfo.isNone and not x.key.checkSatisfied), PackageReference]

          if unsatisfied.len > 0:
            removeTmp()
            printUnsatisfied(config, satisfied, unsatisfied)
            1
          else:
            proc installNext(index: int, installedAs: List[(string, string)],
              lastCode: int): (Table[string, string], int, int) =
              if index < basePackages.len and lastCode == 0:
                let (addInstalledAs, code) = installGroupFromSources(config, commonArgs,
                  basePackages[index], explicits, noconfirm)
                installNext(index + 1, addInstalledAs ^& installedAs, code)
              else:
                (toSeq(installedAs.items).toTable, lastCode, index - 1)

            let (installedAs, code, index) = installNext(0, nil, 0)
            if code != 0 and index < basePackages.len - 1:
              printWarning(config.color, tr"installation aborted")
            removeTmp()

            let newKeepNames = keepNames.map(n => installedAs.opt(n).get(n))
            let (_, finalUnrequired, finalUnrequiredWithoutOptional) = withAlpm(config.root,
              config.db, newSeq[string](), config.arch, handle, dbs, errors):
              queryUnrequired(handle, true, true, newKeepNames)

            let unrequired = finalUnrequired - initialUnrequired
            let unrequiredOptional = finalUnrequiredWithoutOptional -
              initialUnrequiredWithoutOptional - unrequired

            let removeCode = if unrequired.len > 0 or unrequiredOptional.len > 0: (block:
                let code = if unrequired.len > 0: (block:
                    printColon(config.color, tr"Removing build dependencies...")
                    pacmanRun(true, config.color, commonArgs &
                      ("R", none(string), ArgumentType.short) &
                      toSeq(unrequired.items).map(t =>
                        (t, none(string), ArgumentType.target))))
                  else:
                    0

                if code == 0 and unrequiredOptional.len > 0:
                  printColon(config.color, tr"Removing optional build dependencies...")
                  pacmanRun(true, config.color, commonArgs &
                    ("R", none(string), ArgumentType.short) &
                    toSeq(unrequiredOptional.items).map(t =>
                      (t, none(string), ArgumentType.target)))
                else:
                  code)
              else:
                0

            if removeCode != 0:
              removeCode
            else:
              code
        elif not directSome and not additionalSome:
          echo(trp(" there is nothing to do\n"))
          0
        else:
          0

proc handlePrint(args: seq[Argument], config: Config, printFormat: string, upgradeCount: int,
  satisfied: Table[PackageReference, SatisfyResult], unsatisfied: seq[PackageReference],
  directPacmanTargets: seq[string], additionalPacmanTargets: seq[string],
  basePackages: seq[seq[seq[PackageInfo]]]): int =

  let code = if directPacmanTargets.len > 0 or
    additionalPacmanTargets.len > 0 or upgradeCount > 0: (block:
      let pacmanTargets = if unsatisfied.len > 0:
          directPacmanTargets
        else:
          directPacmanTargets & additionalPacmanTargets

      let code = pacmanRun(false, config.color, args.filter(arg => not arg.isTarget) &
        pacmanTargets.map(t => (t, none(string), ArgumentType.target)))

      if unsatisfied.len > 0:
        printUnsatisfied(config, satisfied, unsatisfied)
        1
      else:
        code)
    else:
      0

  if code == 0:
    proc printWithFormat(pkgInfo: PackageInfo) =
      echo(printFormat
        .replace("%n", pkgInfo.name)
        .replace("%v", pkgInfo.version)
        .replace("%r", "aur")
        .replace("%s", "0")
        .replace("%l", pkgInfo.gitUrl))

    for installGroup in basePackages:
      for pkgInfos in installGroup:
        for pkgInfo in pkgInfos:
          printWithFormat(pkgInfo)
    0
  else:
    code

proc filterIgnoresAndConflicts(config: Config, pkgInfos: seq[PackageInfo],
  targetNamesSet: HashSet[string], installed: Table[string, Installed],
  printMode: bool, noconfirm: bool): (seq[PackageInfo], seq[PackageInfo]) =
  let acceptedPkgInfos = pkgInfos.filter(pkgInfo => (block:
    let instGroups = lc[x | (i <- installed.opt(pkgInfo.name),
      x <- i.groups), string]

    if config.ignored(pkgInfo.name, (instGroups & pkgInfo.groups).deduplicate):
      if pkgInfo.name in targetNamesSet:
        if not printMode:
          let input = printColonUserChoice(config.color,
            trp"%s is in IgnorePkg/IgnoreGroup. Install anyway?" % [pkgInfo.name],
            ['y', 'n'], 'y', 'n', noconfirm, 'y')
          input != 'n'
        else:
          true
      else:
        false
    else:
      true))

  let nonConflicingPkgInfos = acceptedPkgInfos.foldl(block:
    let conflictsWith = lc[p | (p <- a, p.name != b.name and
      (lc[0 | (c <- b.conflicts, c.isProvidedBy(p.toPackageReference)), int].len > 0 or
        lc[0 | (c <- p.conflicts, c.isProvidedBy(b.toPackageReference)), int].len > 0)),
      PackageInfo]
    if not printMode and conflictsWith.len > 0:
      for conflict in conflictsWith:
        printWarning(config.color,
          tra("removing '%s' from target list because it conflicts with '%s'\n") %
          [b.name, conflict.name])
      a
    else:
      a & b,
    newSeq[PackageInfo]())

  (nonConflicingPkgInfos, acceptedPkgInfos)

proc deduplicatePkgInfos(targets: seq[PackageInfo],
  config: Config, printMode: bool): seq[PackageInfo] =
  targets.foldl(block:
    if a.map(t => t.name).contains(b.name):
      if not printMode:
        printWarning(config.color, trp("skipping target: %s\n") % [b.name])
      a
    else:
      a & b,
    newSeq[PackageInfo]())

proc checkNeeded(installed: Table[string, Installed],
  name: string, version: string, downgrade: bool): tuple[needed: bool, vercmp: int] =
  if installed.hasKey(name):
    let i = installed[name]
    let vercmp = vercmp(version, i.version)
    let needed = if downgrade: vercmp != 0 else: vercmp > 0
    (needed, vercmp.int)
  else:
    (true, 0)

proc obtainAurPackageInfos(config: Config, rpcInfos: seq[RpcPackageInfo],
  rpcAurTargets: seq[FullPackageTarget[RpcPackageInfo]],
  installed: Table[string, Installed], printMode: bool, needed: bool,
  upgradeCount: int): (seq[PackageInfo], seq[Installed], seq[LocalIsNewer], seq[string]) =
  let targetRpcInfoPairs: seq[tuple[rpcInfo: RpcPackageInfo, upgradeable: bool]] =
    rpcAurTargets.map(t => t.pkgInfo.get).map(i => (i, installed
      .checkNeeded(i.name, i.version, true).needed))

  let upToDateNeeded: seq[Installed] = if needed:
      targetRpcInfoPairs.map(pair => (block:
        if not pair.upgradeable:
          some(installed[pair.rpcInfo.name])
        else:
          none(Installed)))
      .filter(i => i.isSome)
      .map(i => i.unsafeGet)
    else:
      @[]

  let installedUpgradeRpcInfos = rpcInfos.filter(i => upgradeCount > 0 and (block:
    let reference = i.toPackageReference
    rpcAurTargets.filter(t => t.reference.isProvidedBy(reference)).len == 0))

  let upgradeStructs: seq[tuple[rpcInfo: RpcPackageInfo, needed: bool,
    localIsNewer: Option[LocalIsNewer]]] = installedUpgradeRpcInfos
    .map(i => (block:
      let res = installed.checkNeeded(i.name, i.version, upgradeCount >= 2)
      let gitPackage = i.name.len > 4 and i.name[i.name.len - 4 .. i.name.len - 1] == "-git"
      let (newNeeded, localIsNewer) = if gitPackage:
          # Don't warn about newer local git packages and don't downgrade them
          (installed.checkNeeded(i.name, i.version, false).needed, none(LocalIsNewer))
        elif not res.needed and res.vercmp < 0:
          (res.needed, some((i.name, installed[i.name].version, i.version)))
        else:
          (res.needed, none(LocalIsNewer))
      (i, newNeeded, localIsNewer)))

  let targetRpcInfos = targetRpcInfoPairs
    .filter(i => not needed or i.upgradeable).map(i => i.rpcInfo)
  let upgradeRpcInfos = upgradeStructs.filter(p => p.needed).map(p => p.rpcInfo)
  let fullRpcInfos = targetRpcInfos & upgradeRpcInfos

  let (pkgInfos, errors) = getAurPackageInfo(fullRpcInfos.map(i => i.name),
    some(fullRpcInfos), config.arch, proc (a: int, b: int) = discard)

  let localIsNewerSeq = upgradeStructs
    .filter(p => p.localIsNewer.isSome)
    .map(p => p.localIsNewer.unsafeGet)

  (pkgInfos, upToDateNeeded, localIsNewerSeq, errors)

proc handleSyncInstall*(args: seq[Argument], config: Config): int =
  let (_, callArgs) = checkAndRefresh(config.color, args)

  let upgradeCount = args.count((some("u"), "sysupgrade"))
  let needed = args.check((none(string), "needed"))
  let noaur = args.check((none(string), "noaur"))
  let build = args.check((none(string), "build"))

  let printModeArg = args.check((some("p"), "print"))
  let printModeFormat = args.filter(arg => arg
    .matchOption((none(string), "print-format"))).optLast
  let printFormat = if printModeArg or printModeFormat.isSome:
      some(printModeFormat.map(arg => arg.value.get).get("%l"))
    else:
      none(string)

  let noconfirm = args
    .filter(arg => arg.matchOption((none(string), "confirm")) or
      arg.matchOption((none(string), "noconfirm"))).optLast
    .map(arg => arg.key == "noconfirm").get(false)

  let targets = args.packageTargets

  let (syncTargets, checkAurNames, installed, foreignUpgrade) = withAlpm(config.root, config.db,
    config.dbs, config.arch, handle, dbs, errors):
    for e in errors: printError(config.color, e)

    let (syncTargets, checkAurNames) = findSyncTargets(handle, dbs, targets,
      not build, not build)

    let installed = lc[($p.name, $p.version, p.groupsSeq, p.reason == AlpmReason.explicit) |
      (p <- handle.local.packages), Installed]

    let foreignUpgrade = if upgradeCount > 0: (block:
        proc checkForeignAndIrreplaceable(name: string): bool =
          if dbs.filter(d => d[name] != nil).len > 0:
            return false
          else:
            for db in dbs:
              for pkg in db.packages:
                for replaces in pkg.replaces:
                  if replaces.name == name:
                    return false
            return true

        installed
          .map(i => i.name)
          .filter(checkForeignAndIrreplaceable)
          .toSet)
      else:
        initSet[string]()

    (syncTargets, checkAurNames, installed, foreignUpgrade)

  let checkAurNamesFull = if noaur:
      @[]
    elif upgradeCount > 0:
      installed
        .filter(i => i.name in foreignUpgrade and
          (config.checkIgnored or not config.ignored(i.name, i.groups)))
        .map(i => i.name) & checkAurNames
    else:
      checkAurNames

  withAur():
    if printFormat.isNone and (checkAurNamesFull.len > 0 or build):
      printColon(config.color, tr"Resolving build targets...")
      if checkAurNamesFull.len > 0:
        echo(tr"checking AUR database...")

    let (rpcInfos, rerrors) = getRpcPackageInfo(checkAurNamesFull)
    for e in rerrors: printError(config.color, e)

    let rpcNotFoundTargets = filterNotFoundSyncTargets(syncTargets,
      rpcInfos, initTable[string, PackageReference]())

    if rpcNotFoundTargets.len > 0:
      printSyncNotFound(config, rpcNotFoundTargets)
      1
    else:
      if upgradeCount > 0 and not noaur and printFormat.isNone and config.printAurNotFound:
        let rpcInfoTable = rpcInfos.map(i => (i.name, i)).toTable
        for inst in installed:
          if inst.name in foreignUpgrade and not config.ignored(inst.name, inst.groups) and
            not rpcInfoTable.hasKey(inst.name):
            printWarning(config.color, tr"$# was not found in AUR" % [inst.name])

      let installedTable = installed.map(i => (i.name, i)).toTable
      let rpcAurTargets = mapAurTargets(syncTargets, rpcInfos).filter(isAurTargetFull)

      let (aurPkgInfos, upToDateNeeded, localIsNewerSeq, aperrors) = obtainAurPackageInfos(config,
        rpcInfos, rpcAurTargets, installedTable, printFormat.isSome, needed, upgradeCount)
      for e in aperrors: printError(config.color, e)

      let upToDateNeededTable = upToDateNeeded.map(i => (i.name,
        (i.name, none(string), some((ConstraintOperation.eq, i.version))))).toTable
      let notFoundTargets = filterNotFoundSyncTargets(syncTargets,
        aurPkgInfos, upToDateNeededTable)

      if notFoundTargets.len > 0:
        printSyncNotFound(config, notFoundTargets)
        1
      else:
        let fullTargets = mapAurTargets(syncTargets.filter(t => not (upToDateNeededTable
          .opt(t.reference.name).map(r => t.reference.isProvidedBy(r)).get(false))), aurPkgInfos)
        let pacmanTargets = fullTargets.filter(t => not isAurTargetFull(t))
        let aurTargets = fullTargets.filter(isAurTargetFull)

        if printFormat.isNone and upgradeCount == 1:
          for localIsNewer in localIsNewerSeq:
            printWarning(config.color, tra("%s: local (%s) is newer than %s (%s)\n") %
              [localIsNewer.name, localIsNewer.version, "aur", localIsNewer.aurVersion])

        if printFormat.isNone:
          for inst in upToDateNeeded:
            printWarning(config.color, tra("%s-%s is up to date -- skipping\n") %
              [inst.name, inst.version])

        let neededPacmanTargets = if printFormat.isNone and build and needed:
            pacmanTargets.filter(target => (block:
              let version = target.foundInfos[0].pkg.get.version
              if installedTable.checkNeeded(target.reference.name, version, true).needed:
                true
              else:
                printWarning(config.color, tra("%s-%s is up to date -- skipping\n") %
                  [target.reference.name, version])
                false))
          else:
            pacmanTargets

        let checkPacmanPkgInfos = printFormat.isNone and build and
          neededPacmanTargets.len > 0

        let (buildPkgInfos, obtainErrorMessages) = if checkPacmanPkgInfos: (block:
            echo(tr"checking official repositories...")
            obtainBuildPkgInfos[PackageInfo](config, pacmanTargets))
          else:
            (@[], @[])

        if checkPacmanPkgInfos and buildPkgInfos.len < pacmanTargets.len:
          # "--build" conflicts with "--sysupgrade", so it's ok to fail here
          for e in obtainErrorMessages: printError(config.color, e)
          1
        else:
          let pkgInfos = buildPkgInfos & aurPkgInfos
          let targetNamesSet = (pacmanTargets & aurTargets).map(t => t.reference.name).toSet
          let (finalPkgInfos, acceptedPkgInfos) = filterIgnoresAndConflicts(config, pkgInfos,
            targetNamesSet, installedTable, printFormat.isSome, noconfirm)

          if finalPkgInfos.len > 0 and printFormat.isNone:
            echo(trp("resolving dependencies...\n"))
          let (satisfied, unsatisfied) = withAlpm(config.root, config.db,
            config.dbs, config.arch, handle, dbs, errors):
            findDependencies(config, handle, dbs, finalPkgInfos, printFormat.isSome, noaur)

          if printFormat.isNone:
            let acceptedSet = acceptedPkgInfos.map(i => i.name).toSet

            for pkgInfo in pkgInfos:
              if not (pkgInfo.name in acceptedSet):
                if not (pkgInfo.name in targetNamesSet) and upgradeCount > 0 and
                  installedTable.hasKey(pkgInfo.name):
                  let installedVersion = installedTable[pkgInfo.name].version
                  let newVersion = pkgInfo.version
                  if vercmp(newVersion, installedVersion) < 0:
                    printWarning(config.color, tra("%s: ignoring package downgrade (%s => %s)\n") %
                      [pkgInfo.name, installedVersion, newVersion])
                  else:
                    printWarning(config.color, tra("%s: ignoring package upgrade (%s => %s)\n") %
                      [pkgInfo.name, installedVersion, newVersion])
                else:
                  printWarning(config.color, trp("skipping target: %s\n") % [pkgInfo.name])
              elif pkgInfo.repo == "aur":
                if pkgInfo.maintainer.isNone:
                  printWarning(config.color, tr"$# is orphaned" % [pkgInfo.name])
                if installedTable.hasKey(pkgInfo.name):
                  let installedVersion = installedTable[pkgInfo.name].version
                  let newVersion = pkgInfo.version
                  if vercmp(newVersion, installedVersion) < 0:
                    printWarning(config.color,
                      tra("%s: downgrading from version %s to version %s\n") %
                      [pkgInfo.name, installedVersion, newVersion])

          let buildAndAurNamesSet = finalPkgInfos.map(i => i.name).toSet
          let fullPkgInfos = (finalPkgInfos & lc[i | (s <- satisfied.values,
            i <- s.buildPkgInfo, not (i.name in buildAndAurNamesSet)), PackageInfo])
            .deduplicatePkgInfos(config, printFormat.isSome)

          let directPacmanTargets = pacmanTargets.map(`$`)
          let additionalPacmanTargets = lc[x.name | (x <- satisfied.values,
            not x.installed and x.buildPkgInfo.isNone), string]
          let orderedPkgInfos = orderInstallation(fullPkgInfos, satisfied)

          let pacmanArgs = callArgs.filterExtensions(true, true)
          if printFormat.isSome:
            handlePrint(pacmanArgs, config, printFormat.unsafeGet, upgradeCount,
              satisfied, unsatisfied, directPacmanTargets, additionalPacmanTargets,
              orderedPkgInfos)
          else:
            let foreignInstalled = installed.filter(i => i.name in foreignUpgrade)
            let foreignExplicitsNamesSet = foreignInstalled
              .filter(i => i.explicit).map(i => i.name).toSet
            let foreignDepsNamesSet = foreignInstalled
              .filter(i => not i.explicit).map(i => i.name).toSet
            let keepNames = foreignExplicitsNamesSet + foreignDepsNamesSet + targetNamesSet

            let explicits = if args.check((none(string), "asexplicit")):
                targetNamesSet + foreignExplicitsNamesSet + foreignDepsNamesSet
              elif args.check((none(string), "asdeps")):
                initSet[string]()
              else:
                foreignExplicitsNamesSet + (targetNamesSet - foreignDepsNamesSet)

            handleInstall(pacmanArgs, config, upgradeCount, noconfirm,
              explicits, installed, satisfied, unsatisfied,
              keepNames, build, directPacmanTargets, additionalPacmanTargets,
              orderedPkgInfos)

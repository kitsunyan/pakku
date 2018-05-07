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

proc createCloneProgress(config: Config, count: int, flexible: bool, printMode: bool):
  (proc (update: int, terminate: int) {.closure.}, proc {.closure.}) =
  if count >= 1 and not printMode:
    let (update, terminate) = printProgressShare(config.progressBar, tr"cloning repositories")
    update(0, count)

    if flexible:
      proc cloneUpdate(progress: int, newCount: int) {.closure.} =
        # newCount can be < count if some packages were not found
        update(max(count - newCount + progress, 0), count)

      (cloneUpdate, terminate)
    else:
      (update, terminate)
  else:
    (proc (a: int, b: int) {.closure.} = discard, proc {.closure.} = discard)

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
  totalAurFail: seq[PackageReference], additionalPkgInfos: seq[PackageInfo], paths: seq[string],
  nodepsCount: int, assumeInstalled: seq[PackageReference], printMode: bool, noaur: bool):
  (Table[PackageReference, SatisfyResult], seq[PackageReference], seq[string]) =
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
        if satref == reference or reference
          .isProvidedBy(pkgInfo.toPackageReference, nodepsCount == 0):
          return some(pkgInfo)
        for provides in pkgInfo.provides:
          if reference.isProvidedBy(provides, nodepsCount == 0) and
            checkDependencyCycle(pkgInfo, reference):
            return some(pkgInfo)
    return none(PackageInfo)

  proc findInAdditional(reference: PackageReference): Option[PackageInfo] =
    for pkgInfo in additionalPkgInfos:
      if reference.isProvidedBy(pkgInfo.toPackageReference, nodepsCount == 0):
        return some(pkgInfo)
      for provides in pkgInfo.provides:
        if reference.isProvidedBy(provides, nodepsCount == 0) and
          checkDependencyCycle(pkgInfo, reference):
          return some(pkgInfo)
    return none(PackageInfo)

  proc findInDatabaseWithGroups(db: ptr AlpmDatabase, reference: PackageReference,
    directName: bool): Option[tuple[name: string, groups: seq[string]]] =
    for pkg in db.packages:
      if reference.isProvidedBy(pkg.toPackageReference, nodepsCount == 0):
        return some(($pkg.name, pkg.groupsSeq))
      for provides in pkg.provides:
        if reference.isProvidedBy(provides.toPackageReference, nodepsCount == 0):
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
      if nodepsCount >= 2 or
        assumeInstalled.filter(r => reference.isProvidedBy(r, true)).len > 0:
        some((true, reference.name, none(PackageInfo)))
      else:
        let pkgInfo = findInSatisfied(reference)
        if pkgInfo.isSome:
          some((false, pkgInfo.unsafeGet.name, pkgInfo))
        else:
          let pkgInfo = findInAdditional(reference)
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

  let (aurSuccess, aurFail, newPaths, newAdditionalPkgInfos) =
    if not noaur and aurCheck.len > 0: (block:
      let (update, terminate) = createCloneProgress(config, aurCheck.len, true, printMode)
      try:
        withAur():
          let (pkgInfos, additionalPkgInfos, paths) = if printMode: (block:
              let (pkgInfos, additionalPkgInfos, aerrors) =
                getAurPackageInfos(aurCheck.map(r => r.name), config.arch)
              for e in aerrors: printError(config.color, e)
              (pkgInfos, additionalPkgInfos, newSeq[string]()))
            else: (block:
              let (rpcInfos, aerrors) = getRpcPackageInfos(aurCheck.map(r => r.name))
              for e in aerrors: printError(config.color, e)
              let (pkgInfos, additionalPkgInfos, paths, cerrors) =
                cloneAurReposWithPackageInfos(config, rpcInfos, not printMode, update, true)
              for e in cerrors: printError(config.color, e)
              (pkgInfos, additionalPkgInfos, paths))

          let acceptedPkgInfos = pkgInfos.filter(i => not config.ignored(i.name, i.groups))
          let aurTable = acceptedPkgInfos.map(i => (i.name, i)).toTable
          let aurResult = aurCheck.map(proc (reference: PackageReference): ReferenceResult =
            if aurTable.hasKey(reference.name):
              (reference, some((false, reference.name, some(aurTable[reference.name]))))
            else:
              (reference, none(SatisfyResult)))

          let aurSuccess = aurResult.filter(r => r.result.isSome)
          let aurFail = aurResult.filter(r => r.result.isNone).map(r => r.reference)
          (aurSuccess, aurFail, paths, additionalPkgInfos)
      finally:
        terminate())
    else:
      (@[], aurCheck, @[], @[])

  let newSatisfied = (toSeq(satisfied.pairs) &
    success.map(r => (r.reference, r.result.unsafeGet)) &
    aurSuccess.map(r => (r.reference, r.result.unsafeGet))).toTable

  let newUnsatisfied = lc[x | (y <- aurSuccess, r <- y.result, i <- r.buildPkgInfo,
    x <- i.allDepends), PackageReference].deduplicate

  let newTotalAurFail = (totalAurFail & aurFail).deduplicate
  let newTotalUnsatisfied = (newUnsatisfied & newTotalAurFail).deduplicate

  if newUnsatisfied.len > 0:
    findDependencies(config, handle, dbs, newSatisfied, newTotalUnsatisfied, newTotalAurFail,
      additionalPkgInfos & newAdditionalPkgInfos, paths & newPaths,
      nodepsCount, assumeInstalled, printMode, noaur)
  else:
    let finallyUnsatisfied = newTotalAurFail.filter(r => not newSatisfied.hasKey(r))
    (newSatisfied, finallyUnsatisfied, paths & newPaths)

proc findDependencies(config: Config, handle: ptr AlpmHandle,
  dbs: seq[ptr AlpmDatabase], pkgInfos: seq[PackageInfo], additionalPkgInfos: seq[PackageInfo],
  nodepsCount: int, assumeInstalled: seq[PackageReference], printMode: bool, noaur: bool):
  (Table[PackageReference, SatisfyResult], seq[PackageReference], seq[string]) =
  let satisfied = pkgInfos.map(p => ((p.name, none(string), none(VersionConstraint)),
    (false, p.name, some(p)))).toTable
  let unsatisfied = lc[x | (i <- pkgInfos, x <- i.allDepends), PackageReference].deduplicate
  findDependencies(config, handle, dbs, satisfied, unsatisfied, @[],
    additionalPkgInfos, @[], nodepsCount, assumeInstalled, printMode, noaur)

template clearPaths(paths: untyped) =
  for path in paths:
    removeDirQuiet(path)
  discard rmdir(config.tmpRootInitial)

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

template dropPrivilegesAndChdir(path: Option[string], body: untyped): int =
  if dropPrivileges():
    if path.isNone or chdir(path.unsafeGet) == 0:
      body
    else:
      printError(config.color, tr"chdir failed: $#" % [path.unsafeGet])
      quit(1)
  else:
    printError(config.color, tr"failed to drop privileges")
    quit(1)

proc editLoop(config: Config, base: string, repoPath: string, gitSubdir: Option[string],
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
        discard forkWait(() => (block:
          let buildPath = buildPath(repoPath, gitSubdir)
          dropPrivilegesAndChdir(some(buildPath)):
            execResult(bashCmd, "-c", """$1 "$2"""", "bash", editor, file)))
        editFileLoop(file)
    else:
      res

  let rawFiles = getGitFiles(repoPath, gitSubdir, true)
  let files = ("PKGBUILD" & rawFiles.filter(x => x != ".SRCINFO")).deduplicate

  proc editFileLoopAll(index: int): char =
    if index < files.len:
      let res = editFileLoop(files[index])
      if res == 'n': editFileLoopAll(index + 1) else: res
    else:
      'n'

  editFileLoopAll(0)

proc buildLoop(config: Config, pkgInfos: seq[PackageInfo], skipDeps: bool,
  noconfirm: bool, noextract: bool): (Option[BuildResult], int, bool) =
  let base = pkgInfos[0].base
  let repoPath = repoPath(config.tmpRootInitial, base)
  let gitSubdir = pkgInfos[0].gitSubdir
  let buildPath = buildPath(repoPath, gitSubdir)

  let confFileEnv = getEnv("MAKEPKG_CONF")
  let confFile = if confFileEnv.len == 0:
      sysConfDir & "/makepkg.conf"
    else:
      confFileEnv

  let workConfFile = config.tmpRootInitial & "/makepkg.conf"

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
        file.writeLine("PKGDEST=" & config.tmpRootInitial.bashEscape)
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
          dropPrivilegesAndChdir(none(string)):
            execResult(bashCmd, "-c",
              "source \"$@\" && echo \"$PKGEXT\"",
              "bash", workConfFile)))
          .output.optFirst.get("")
      else:
        envExt

    let (buildCode, interrupted) = catchInterrupt():
      forkWait(proc: int =
        discard cunsetenv("MAKEPKG_CONF")
        dropPrivilegesAndChdir(some(buildPath)):
          if not noextract:
            removeDirQuiet(buildPath & "src")

          let optional: seq[tuple[arg: string, cond: bool]] = @[
            ("-e", noextract),
            ("-m", not config.color),
            ("-d", skipDeps)
          ]

          execResult(@[makepkgCmd, "--config", workConfFile, "-f"] &
            optional.filter(o => o.cond).map(o => o.arg)))

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
  pkgInfos: seq[PackageInfo], skipDeps: bool, noconfirm: bool): (Option[BuildResult], int) =
  let base = pkgInfos[0].base
  let repoPath = repoPath(config.tmpRootInitial, base)
  let gitSubdir = pkgInfos[0].gitSubdir

  proc loop(noextract: bool, showEditLoop: bool): (Option[BuildResult], int) =
    let res = if showEditLoop and not noconfirm:
        editLoop(config, base, repoPath, gitSubdir, false, noconfirm)
      else:
        'n'

    if res == 'a':
      (none(BuildResult), 1)
    else:
      let (buildResult, code, interrupted) = buildLoop(config, pkgInfos,
        skipDeps, noconfirm, noextract)

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
        dropPrivilegesAndChdir(some(buildPath(repoPath, gitSubdir))):
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
  skipDeps: bool, noconfirm: bool): (seq[(string, string)], int) =
  proc buildNext(index: int, buildResults: List[BuildResult]): (List[BuildResult], int) =
    if index < basePackages.len:
      let (buildResult, code) = buildFromSources(config, commonArgs,
        basePackages[index], skipDeps, noconfirm)

      if code != 0:
        (buildResults.reversed, code)
      else:
        buildNext(index + 1, buildResult.unsafeGet ^& buildResults)
    else:
      (buildResults.reversed, 0)

  let (buildResults, buildCode) = buildNext(0, nil)

  proc formatArchiveFile(pkgInfo: PackageInfo, ext: string): string =
    let arch = if config.arch in pkgInfo.archs: config.arch else: "any"
    config.tmpRootInitial & "/" & pkgInfo.name & "-" & pkgInfo.version & "-" & arch & ext

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
      printWarning(config.color, tr"packages are saved to '$#'" % [config.tmpRootInitial])

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
      let pacmanParams = pacmanCmd & pacmanParams(config.color,
        commonArgs & ("U", none(string), ArgumentType.short))
      let asdeps = install.filter(p => not (p.name in explicits)).map(p => p.file)
      let asexplicit = install.filter(p => p.name in explicits).map(p => p.file)

      let installParams = sudoPrefix & (pkgLibDir & "/install") & config.cache &
        $pacmanParams.len & pacmanParams & $asdeps.len & asdeps & $asexplicit.len & asexplicit

      let code = forkWait(() => execResult(installParams))
      if code != 0:
        handleTmpRoot(false)
        (newSeq[(string, string)](), code)
      else:
        handleTmpRoot(true)
        let installedAs = lc[(r.name.unsafeGet, r.pkgInfo.name) | (br <- buildResults,
          r <- br.replacePkgInfos, r.name.isSome), (string, string)]
        (installedAs, 0)

proc deduplicatePkgInfos(pkgInfos: seq[PackageInfo],
  config: Config, printWarning: bool): seq[PackageInfo] =
  pkgInfos.foldl(block:
    if a.map(t => t.name).contains(b.name):
      if printWarning:
        printWarning(config.color, trp("skipping target: %s\n") % [b.name])
      a
    else:
      a & b,
    newSeq[PackageInfo]())

proc resolveDependencies(config: Config, pkgInfos: seq[PackageInfo],
  additionalPkgInfos: seq[PackageInfo], printMode: bool, directSome: bool,
  nodepsCount: int, assumeInstalled: seq[PackageReference], noaur: bool):
  (bool, Table[PackageReference, SatisfyResult],
  seq[string], seq[seq[seq[PackageInfo]]], seq[string]) =
  if pkgInfos.len > 0 and not printMode:
    if directSome:
      printColon(config.color, tr"Resolving build targets...")
    echo(trp("resolving dependencies...\n"))
  let (satisfied, unsatisfied, paths) = withAlpm(config.root, config.db,
    config.dbs, config.arch, handle, dbs, errors):
    findDependencies(config, handle, dbs, pkgInfos, additionalPkgInfos,
      nodepsCount, assumeInstalled, printMode, noaur)

  if unsatisfied.len > 0:
    clearPaths(paths)
    printUnsatisfied(config, satisfied, unsatisfied)
    (false, satisfied, newSeq[string](),
      newSeq[seq[seq[PackageInfo]]](), newSeq[string]())
  else:
    let buildAndAurNamesSet = pkgInfos.map(i => i.name).toSet
    let fullPkgInfos = (pkgInfos & lc[i | (s <- satisfied.values,
      i <- s.buildPkgInfo, not (i.name in buildAndAurNamesSet)), PackageInfo])
      .deduplicatePkgInfos(config, false)

    let additionalPacmanTargets = lc[x.name | (x <- satisfied.values,
      not x.installed and x.buildPkgInfo.isNone), string]
    let orderedPkgInfos = orderInstallation(fullPkgInfos, satisfied)

    (true, satisfied, additionalPacmanTargets, orderedPkgInfos, paths)

proc confirmViewAndImportKeys(config: Config, basePackages: seq[seq[seq[PackageInfo]]],
  installed: seq[Installed], noconfirm: bool): int =
  if basePackages.len > 0: (block:
    let installedVersions = installed.map(i => (i.name, i.version)).toTable

    printPackages(config.color, config.verbosePkgList,
      lc[(i.name, i.repo, installedVersions.opt(i.name), i.version) |
        (g <- basePackages, b <- g, i <- b), PackageInstallFormat]
        .sorted((a, b) => cmp(a.name, b.name)))
    let input = printColonUserChoice(config.color,
      tr"Proceed with building?", ['y', 'n'], 'y', 'n', noconfirm, 'y')

    if input == 'y':
      let flatBasePackages = lc[x | (a <- basePackages, x <- a), seq[PackageInfo]]

      proc checkNext(index: int, skipEdit: bool, skipKeys: bool): int =
        if index < flatBasePackages.len:
          let pkgInfos = flatBasePackages[index]
          let base = pkgInfos[0].base
          let repoPath = repoPath(config.tmpRootInitial, base)

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
              editLoop(config, base, repoPath, pkgInfos[0].gitSubdir, defaultYes, noconfirm))

          if editRes == 'a':
            1
          else:
            let resultPkgInfos = reloadPkgInfos(config,
              repoPath & "/" & pkgInfos[0].gitSubdir.get("."), pkgInfos)
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
                dropPrivilegesAndChdir(none(string)):
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
                    dropPrivilegesAndChdir(none(string)):
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

      checkNext(0, false, false)
    else:
      1)
  else:
    0

proc removeBuildDependencies(config: Config, commonArgs: seq[Argument],
  unrequired: HashSet[string], unrequiredOptional: HashSet[string]): int =
  if unrequired.len > 0 or unrequiredOptional.len > 0: (block:
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

proc assumeInstalled(args: seq[Argument]): seq[PackageReference] =
  args
    .filter(a => a.matchOption(%%%"assume-installed"))
    .map(a => a.value.get.parsePackageReference(false))
    .filter(r => r.constraint.isNone or
      r.constraint.unsafeGet.operation == ConstraintOperation.eq)

proc handleInstall(args: seq[Argument], config: Config, upgradeCount: int, nodepsCount: int,
  noconfirm: bool, explicits: HashSet[string], installed: seq[Installed],
  pacmanTargets: seq[FullPackageTarget[PackageInfo]], pkgInfos: seq[PackageInfo],
  additionalPkgInfos: seq[PackageInfo], keepNames: HashSet[string],
  initialPaths: seq[string], build: bool, noaur: bool): int =
  let workDirectPacmanTargets = if build: @[] else: pacmanTargets.map(`$`)

  let (directCode, directSome) = if workDirectPacmanTargets.len > 0 or upgradeCount > 0:
      (pacmanRun(true, config.color, args.filter(arg => not arg.isTarget) &
        workDirectPacmanTargets.map(t => (t, none(string), ArgumentType.target))), true)
    else:
      (0, false)

  if directCode != 0:
    clearPaths(initialPaths)
    directCode
  else:
    let commonArgs = args
      .keepOnlyOptions(commonOptions, transactionOptions, upgradeOptions)
      .filter(true, false, %%%"asdeps", %%%"asexplicit", %%%"needed")

    let assumeInstalled = args.assumeInstalled
    let skipDeps = assumeInstalled.len > 0 or nodepsCount > 0

    let (resolveSuccess, satisfied, additionalPacmanTargets, basePackages, dependencyPaths) =
      resolveDependencies(config, pkgInfos, additionalPkgInfos, false, directSome,
        nodepsCount, assumeInstalled, noaur)
    let paths = initialPaths & dependencyPaths

    let confirmAndResolveCode = if resolveSuccess:
        confirmViewAndImportKeys(config, basePackages, installed, noconfirm)
      else:
        1

    if confirmAndResolveCode != 0:
      clearPaths(paths)
      confirmAndResolveCode
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
        clearPaths(paths)
        additionalCode
      else:
        if basePackages.len > 0:
          # check all pacman dependencies were installed
          let unsatisfied = if nodepsCount <= 1:
              withAlpm(config.root, config.db,
                config.dbs, config.arch, handle, dbs, errors):
                for e in errors: printError(config.color, e)

                proc checkSatisfied(reference: PackageReference): bool =
                  for pkg in handle.local.packages:
                    if reference.isProvidedBy(pkg.toPackageReference, nodepsCount == 0):
                      return true
                    for provides in pkg.provides:
                      if reference.isProvidedBy(provides.toPackageReference, nodepsCount == 0):
                        return true
                  return false

                lc[x.key | (x <- satisfied.namedPairs, not x.value.installed and
                  x.value.buildPkgInfo.isNone and not x.key.checkSatisfied), PackageReference]
            else:
              @[]

          if unsatisfied.len > 0:
            clearPaths(paths)
            printUnsatisfied(config, satisfied, unsatisfied)
            1
          else:
            proc installNext(index: int, installedAs: List[(string, string)],
              lastCode: int): (Table[string, string], int, int) =
              if index < basePackages.len and lastCode == 0:
                let (addInstalledAs, code) = installGroupFromSources(config, commonArgs,
                  basePackages[index], explicits, skipDeps, noconfirm)
                installNext(index + 1, addInstalledAs ^& installedAs, code)
              else:
                (toSeq(installedAs.items).toTable, lastCode, index - 1)

            let (installedAs, code, index) = installNext(0, nil, 0)
            if code != 0 and index < basePackages.len - 1:
              printWarning(config.color, tr"installation aborted")
            clearPaths(paths)

            let newKeepNames = keepNames.map(n => installedAs.opt(n).get(n))
            let (_, finalUnrequired, finalUnrequiredWithoutOptional) = withAlpm(config.root,
              config.db, newSeq[string](), config.arch, handle, dbs, errors):
              queryUnrequired(handle, true, true, newKeepNames)

            let unrequired = finalUnrequired - initialUnrequired
            let unrequiredOptional = finalUnrequiredWithoutOptional -
              initialUnrequiredWithoutOptional - unrequired

            let removeCode = removeBuildDependencies(config,
              commonArgs, unrequired, unrequiredOptional)
            if removeCode != 0:
              removeCode
            else:
              code
        elif not directSome and not additionalSome:
          echo(trp(" there is nothing to do\n"))
          0
        else:
          0

proc handlePrint(args: seq[Argument], config: Config, printFormat: string,
  upgradeCount: int, nodepsCount: int, pacmanTargets: seq[FullPackageTarget],
  pkgInfos: seq[PackageInfo], additionalPkgInfos: seq[PackageInfo], noaur: bool): int =
  let directPacmanTargets = pacmanTargets.map(`$`)

  let (resolveSuccess, _, additionalPacmanTargets, basePackages, _) =
    resolveDependencies(config, pkgInfos, additionalPkgInfos, true, false,
      nodepsCount, args.assumeInstalled, noaur)

  let code = if directPacmanTargets.len > 0 or
    additionalPacmanTargets.len > 0 or upgradeCount > 0: (block:
      let callPacmanTargets = if resolveSuccess:
          directPacmanTargets & additionalPacmanTargets
        else:
          directPacmanTargets

      # workaround for a strange nim bug, callPacmanTargets.map(...) breaks main.nim
      var callArguments = newSeq[Argument]()
      for t in callPacmanTargets:
        callArguments &= (t, none(string), ArgumentType.target)

      let code = pacmanRun(false, config.color,
        args.filter(arg => not arg.isTarget) & callArguments)

      if resolveSuccess:
        code
      else:
        1)
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

proc printAllWarnings(config: Config, installed: seq[Installed],
  installedTable: Table[string, Installed], rpcInfos: seq[RpcPackageInfo],
  pkgInfos: seq[PackageInfo], acceptedPkgInfos: seq[PackageInfo],
  upToDateNeeded: seq[Installed], buildUpToDateNeeded: seq[(string, string)],
  foreignUpgrade: HashSet[string], localIsNewerSeq: seq[LocalIsNewer],
  targetNamesSet: HashSet[string], upgradeCount: int, noaur: bool) =
  let acceptedSet = acceptedPkgInfos.map(i => i.name).toSet

  if upgradeCount > 0 and not noaur and config.printAurNotFound:
    let rpcInfoTable = rpcInfos.map(i => (i.name, i)).toTable
    for inst in installed:
      if inst.name in foreignUpgrade and not config.ignored(inst.name, inst.groups) and
        not rpcInfoTable.hasKey(inst.name):
        printWarning(config.color, tr"$# was not found in AUR" % [inst.name])

  if upgradeCount == 1:
    for localIsNewer in localIsNewerSeq:
      printWarning(config.color, tra("%s: local (%s) is newer than %s (%s)\n") %
        [localIsNewer.name, localIsNewer.version, "aur", localIsNewer.aurVersion])

  for inst in upToDateNeeded:
    printWarning(config.color, tra("%s-%s is up to date -- skipping\n") %
      [inst.name, inst.version])

  for pair in buildUpToDateNeeded:
    let (name, version) = pair
    printWarning(config.color, tra("%s-%s is up to date -- skipping\n") %
      [name, version])

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
      (lc[0 | (c <- b.conflicts, c.isProvidedBy(p.toPackageReference, true)), int].len > 0 or
        lc[0 | (c <- p.conflicts, c.isProvidedBy(b.toPackageReference, true)), int].len > 0)),
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
  rpcAurTargets: seq[FullPackageTarget[RpcPackageInfo]], installed: Table[string, Installed],
  printMode: bool, needed: bool, upgradeCount: int): (seq[PackageInfo], seq[PackageInfo],
  seq[string], seq[Installed], seq[LocalIsNewer], seq[string]) =
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
    rpcAurTargets.filter(t => t.reference.isProvidedBy(reference, true)).len == 0))

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

  let (update, terminate) = createCloneProgress(config, fullRpcInfos.len, true, printMode)

  let (pkgInfos, additionalPkgInfos, paths, errors) = if printMode: (block:
      let (pkgInfos, additionalPkgInfos, aerrors) =
        getAurPackageInfos(fullRpcInfos.map(i => i.name), config.arch)
      (pkgInfos, additionalPkgInfos, newSeq[string](), aerrors.deduplicate))
    else: (block:
      let (rpcInfos, aerrors) = getRpcPackageInfos(fullRpcInfos.map(i => i.name))
      let (pkgInfos, additionalPkgInfos, paths, cerrors) =
        cloneAurReposWithPackageInfos(config, rpcInfos, not printMode, update, true)
      (pkgInfos, additionalPkgInfos, paths, (toSeq(aerrors.items) & cerrors).deduplicate))

  terminate()

  let localIsNewerSeq = upgradeStructs
    .filter(p => p.localIsNewer.isSome)
    .map(p => p.localIsNewer.unsafeGet)

  (pkgInfos, additionalPkgInfos, paths, upToDateNeeded, localIsNewerSeq, errors)

proc obtainPacmanBuildTargets(config: Config, pacmanTargets: seq[FullPackageTarget[PackageInfo]],
  installedTable: Table[string, Installed], printMode: bool, needed: bool, build: bool):
  (bool, seq[PackageInfo], seq[(string, string)], seq[string], seq[string]) =
  let (neededPacmanBuildTargets, buildUpToDateNeeded) = if not printMode and
    build and needed: (block:
      let neededPairs: seq[tuple[target: FullPackageTarget[PackageInfo],
        skipVersion: Option[string]]] = pacmanTargets.map(target => (block:
        let version = target.foundInfos[0].pkg.get.version
        if installedTable.checkNeeded(target.reference.name, version, true).needed:
          (target, none(string))
        else:
          (target, some(version))))

      let neededPacmanBuildTargets = neededPairs
        .filter(p => p.skipVersion.isNone)
        .map(p => p.target)

      let buildUpToDateNeeded = neededPairs
        .filter(p => p.skipVersion.isSome)
        .map(p => (p.target.reference.name, p.skipVersion.unsafeGet))

      (neededPacmanBuildTargets, buildUpToDateNeeded))
    else:
      (pacmanTargets, @[])

  let checkPacmanBuildPkgInfos = not printMode and build and neededPacmanBuildTargets.len > 0

  let (buildPkgInfos, buildPaths, obtainErrorMessages) = if checkPacmanBuildPkgInfos: (block:
      echo(tr"checking official repositories...")
      let (update, terminate) = createCloneProgress(config, pacmanTargets.len, false, printMode)
      let res = obtainBuildPkgInfos[PackageInfo](config, pacmanTargets, update, true)
      terminate()
      res)
    else:
      (@[], @[], @[])

  (checkPacmanBuildPkgInfos, buildPkgInfos, buildUpToDateNeeded, buildPaths, obtainErrorMessages)

proc findSyncTargetsWithInstalled(config: Config, targets: seq[PackageTarget],
  upgradeCount: int, noaur: bool, build: bool): (seq[SyncPackageTarget], seq[string],
  seq[Installed], HashSet[string]) =
  withAlpm(config.root, config.db,
    config.dbs, config.arch, handle, dbs, errors):
    for e in errors: printError(config.color, e)

    let (syncTargets, checkAurNames) = findSyncTargets(handle, dbs, targets,
      not build, not build)

    let installed = lc[($p.name, $p.version, p.groupsSeq, p.reason == AlpmReason.explicit) |
      (p <- handle.local.packages), Installed]

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

    let foreignUpgrade = installed
      .map(i => i.name)
      .filter(checkForeignAndIrreplaceable)
      .toSet

    let checkAurNamesFull = if noaur:
      @[]
    elif upgradeCount > 0:
      installed
        .filter(i => i.name in foreignUpgrade and
          (config.checkIgnored or not config.ignored(i.name, i.groups)))
        .map(i => i.name) & checkAurNames
    else:
      checkAurNames

    (syncTargets, checkAurNamesFull, installed, foreignUpgrade)

proc resolveBuildTargets(config: Config, targets: seq[PackageTarget],
  printMode: bool, upgradeCount: int, noconfirm: bool, needed: bool, noaur: bool, build: bool):
  (int, seq[Installed], HashSet[string], HashSet[string], seq[FullPackageTarget[PackageInfo]],
  seq[PackageInfo], seq[PackageInfo], seq[string]) =
  template errorResult: untyped = (1, newSeq[Installed](), initSet[string](),
    initSet[string](), newSeq[FullPackageTarget[PackageInfo]](),
    newSeq[PackageInfo](), newSeq[PackageInfo](), newSeq[string]())

  let (syncTargets, checkAurNames, installed, foreignUpgrade) =
    findSyncTargetsWithInstalled(config, targets, upgradeCount, noaur, build)

  if not printMode and (checkAurNames.len > 0 or build):
    printColon(config.color, tr"Resolving build targets...")
    if checkAurNames.len > 0:
      echo(tr"checking AUR database...")

  let (rpcInfos, rerrors) = getRpcPackageInfos(checkAurNames)
  for e in rerrors: printError(config.color, e)

  let rpcNotFoundTargets = filterNotFoundSyncTargets(syncTargets,
    rpcInfos, initTable[string, PackageReference]())

  if rpcNotFoundTargets.len > 0:
    printSyncNotFound(config, rpcNotFoundTargets)
    errorResult
  else:
    let installedTable = installed.map(i => (i.name, i)).toTable
    let rpcAurTargets = mapAurTargets(syncTargets, rpcInfos).filter(isAurTargetFull)

    let (aurPkgInfos, additionalPkgInfos, aurPaths, upToDateNeeded, localIsNewerSeq, aperrors) =
      obtainAurPackageInfos(config, rpcInfos, rpcAurTargets, installedTable,
        printMode, needed, upgradeCount)
    for e in aperrors: printError(config.color, e)

    let upToDateNeededTable = upToDateNeeded.map(i => (i.name,
      (i.name, none(string), some((ConstraintOperation.eq, i.version))))).toTable
    let notFoundTargets = filterNotFoundSyncTargets(syncTargets,
      aurPkgInfos, upToDateNeededTable)

    if notFoundTargets.len > 0:
      clearPaths(aurPaths)
      printSyncNotFound(config, notFoundTargets)
      errorResult
    else:
      let fullTargets = mapAurTargets(syncTargets
        .filter(t => not (upToDateNeededTable.opt(t.reference.name)
        .map(r => t.reference.isProvidedBy(r, true)).get(false))), aurPkgInfos)
      let pacmanTargets = fullTargets.filter(t => not isAurTargetFull(t))
      let aurTargets = fullTargets.filter(isAurTargetFull)

      let (checkPacmanBuildPkgInfos, buildPkgInfos, buildUpToDateNeeded, buildPaths,
        obtainBuildErrorMessages) = obtainPacmanBuildTargets(config, pacmanTargets, installedTable,
        printMode, needed, build)

      if checkPacmanBuildPkgInfos and buildPkgInfos.len < pacmanTargets.len:
        # "--build" conflicts with "--sysupgrade", so it's ok to fail here
        clearPaths(buildPaths)
        clearPaths(aurPaths)
        for e in obtainBuildErrorMessages: printError(config.color, e)
        errorResult
      else:
        let pkgInfos = (buildPkgInfos & aurPkgInfos)
          .deduplicatePkgInfos(config, not printMode)
        let targetNamesSet = (pacmanTargets & aurTargets).map(t => t.reference.name).toSet
        let (finalPkgInfos, acceptedPkgInfos) = filterIgnoresAndConflicts(config, pkgInfos,
          targetNamesSet, installedTable, printMode, noconfirm)

        if not printMode:
          printAllWarnings(config, installed, installedTable, rpcInfos,
            pkgInfos, acceptedPkgInfos, upToDateNeeded, buildUpToDateNeeded,
            foreignUpgrade, localIsNewerSeq, targetNamesSet, upgradeCount, noaur)

        (0, installed, foreignUpgrade, targetNamesSet, pacmanTargets,
          finalPkgInfos, additionalPkgInfos, buildPaths & aurPaths)

proc handleSyncInstall*(args: seq[Argument], config: Config): int =
  let (_, callArgs) = checkAndRefresh(config.color, args)

  let upgradeCount = args.count(%%%"sysupgrade")
  let nodepsCount = args.count(%%%"nodeps")
  let needed = args.check(%%%"needed")
  let noaur = args.check(%%%"noaur")
  let build = args.check(%%%"build")

  let printModeArg = args.check(%%%"print")
  let printModeFormat = args.filter(arg => arg.matchOption(%%%"print-format")).optLast
  let printFormat = if printModeArg or printModeFormat.isSome:
      some(printModeFormat.map(arg => arg.value.get).get("%l"))
    else:
      none(string)

  let noconfirm = args
    .filter(arg => arg.matchOption(%%%"confirm") or
      arg.matchOption(%%%"noconfirm")).optLast
    .map(arg => arg.key == "noconfirm").get(false)

  let targets = args.packageTargets(false)

  withAur():
    let (code, installed, foreignUpgrade, targetNamesSet, pacmanTargets,
      pkgInfos, additionalPkgInfos, paths) = resolveBuildTargets(config, targets,
      printFormat.isSome, upgradeCount, noconfirm, needed, noaur, build)

    let pacmanArgs = callArgs.filterExtensions(true, true,
      commonOptions, transactionOptions, upgradeOptions, syncOptions)
    if code != 0:
      code
    elif printFormat.isSome:
      handlePrint(pacmanArgs, config, printFormat.unsafeGet, upgradeCount, nodepsCount,
        pacmanTargets, pkgInfos, additionalPkgInfos, noaur)
    else:
      let foreignInstalled = installed.filter(i => i.name in foreignUpgrade)
      let foreignExplicitsNamesSet = foreignInstalled
        .filter(i => i.explicit).map(i => i.name).toSet
      let foreignDepsNamesSet = foreignInstalled
        .filter(i => not i.explicit).map(i => i.name).toSet
      let keepNames = foreignExplicitsNamesSet + foreignDepsNamesSet + targetNamesSet

      let explicits = if args.check(%%%"asexplicit"):
          targetNamesSet + foreignExplicitsNamesSet + foreignDepsNamesSet
        elif args.check(%%%"asdeps"):
          initSet[string]()
        else:
          foreignExplicitsNamesSet + (targetNamesSet - foreignDepsNamesSet)

      handleInstall(pacmanArgs, config, upgradeCount, nodepsCount, noconfirm,
        explicits, installed, pacmanTargets, pkgInfos, additionalPkgInfos, keepNames,
        paths, build, noaur)

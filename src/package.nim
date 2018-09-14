import
  future, options, os, re, sequtils, sets, strutils, tables, utils,
  "wrapper/alpm"

type
  ConstraintOperation* {.pure.} = enum
    ge = ">=",
    gt = ">",
    eq = "=",
    lt = "<",
    le = "<="

  VersionConstraint* = tuple[
    operation: ConstraintOperation,
    version: string,
    impliedVersion: bool
  ]

  PackageReference* = tuple[
    name: string,
    description: Option[string],
    constraint: Option[VersionConstraint]
  ]

  RpcPackageInfo* = object of RootObj
    repo*: string
    base*: string
    name*: string
    version*: string
    description*: Option[string]
    maintainer*: Option[string]
    firstSubmitted*: Option[int64]
    lastModified*: Option[int64]
    outOfDate*: Option[int64]
    votes*: int
    popularity*: float
    gitUrl*: string
    gitSubdir*: Option[string]

  PackageInfo* = object of RpcPackageInfo
    baseIndex*: int
    baseCount*: int
    archs*: seq[string]
    url*: Option[string]
    licenses*: seq[string]
    groups*: seq[string]
    pgpKeys*: seq[string]
    depends*: seq[PackageReference]
    makeDepends*: seq[PackageReference]
    checkDepends*: seq[PackageReference]
    optional*: seq[PackageReference]
    provides*: seq[PackageReference]
    conflicts*: seq[PackageReference]
    replaces*: seq[PackageReference]

  GitRepo* = tuple[
    url: string,
    bareName: Option[string],
    branch: Option[string],
    path: string
  ]

  PackageRepo = tuple[
    os: HashSet[string],
    repo: HashSet[string],
    git: GitRepo
  ]

  SrcInfoPair = tuple[key: string, value: string]

const
  packageRepos: seq[PackageRepo] = @[
    (["arch", "parabola"].toSet,
      ["core", "extra", "testing"].toSet,
      ("https://git.archlinux.org/svntogit/packages.git", none(string),
        some("packages/${BASE}"), "repos/${REPO}-${ARCH}")),
    (["arch", "parabola"].toSet,
      ["community", "community-testing", "multilib", "multilib-testing"].toSet,
      ("https://git.archlinux.org/svntogit/community.git", none(string),
        some("packages/${BASE}"), "repos/${REPO}-${ARCH}")),
    (["parabola"].toSet,
      ["libre", "libre-testing", "libre-multilib", "libre-multilib-testing",
        "nonprism", "nonprism-testing", "nonprism-multilib", "nonprism-multilib-testing",
        "pcr", "pcr-testing", "pcr-multilib", "pcr-multilib-testing"].toSet,
      ("https://git.parabola.nu/abslibre.git", some("parabola"),
        none(string), "${REPO}/${BASE}"))
  ]

static:
  # test only single match available
  let osSet = lc[x | (r <- packageRepos, x <- r.os), string].toSet
  let repoSet = lc[x | (r <- packageRepos, x <- r.repo), string].toSet
  for os in osSet:
    for repo in repoSet:
      let osValue = os
      let repoValue = repo
      if packageRepos.filter(pr => osValue in pr.os and repoValue in pr.repo).len >= 2:
        raise newException(SystemError,
          "only single matching repo available: " & os & ":" & repo)

  # test unique url <> bareName links
  let bareNameToUrl = lc[(x, r.git.url) |
    (r <- packageRepos, x <- r.git.bareName), (string, string)].toTable
  let urlToBareName = lc[(r.git.url, x) |
    (r <- packageRepos, x <- r.git.bareName), (string, string)].toTable

  template testBareNamesAndUrls(m1: untyped, m2: untyped) =
    for x1, x2 in m1:
      try:
        if m2[x2] != x1:
          raise newException(SystemError, "")
      except:
        raise newException(SystemError, "Invalid url <> bareName links")

  testBareNamesAndUrls(bareNameToUrl, urlToBareName)
  testBareNamesAndUrls(urlToBareName, bareNameToUrl)

proc readOsId: Option[string] =
  var file: File
  if file.open("/usr/lib/os-release"):
    try:
      while true:
        let rawLine = readLine(file)
        if rawLine[0 .. 2] == "ID=":
          return some(rawLine[3 .. ^1])
    except EOFError:
      discard
    except IOError:
      discard
    finally:
      file.close()
  return none(string)

let osId = readOsId()

proc lookupGitRepo*(repo: string, base: string, arch: string): Option[GitRepo] =
  proc replaceAll(gitPart: string): string =
    gitPart
      .replace("${REPO}", repo)
      .replace("${BASE}", base)
      .replace("${ARCH}", arch)

  packageRepos
    .filter(pr => osId.isSome and osid.unsafeGet in pr.os and repo in pr.repo)
    .map(pr => (pr.git.url.replaceAll, pr.git.bareName,
      pr.git.branch.map(replaceAll), pr.git.path.replaceAll))
    .optFirst

template repoPath*(tmpRoot: string, base: string): string =
  tmpRoot & "/" & base

template buildPath*(repoPath: string, gitSubdir: Option[string]): string =
  gitSubdir.map(p => repoPath & "/" & p).get(repoPath)

template allDepends*(pkgInfo: PackageInfo): seq[PackageReference] =
  pkgInfo.depends & pkgInfo.makeDepends & pkgInfo.checkDepends

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

proc isProvidedBy*(package: PackageReference, by: PackageReference, checkVersions: bool): bool =
  if package.name == by.name:
    if not checkVersions or package.constraint.isNone or by.constraint.isNone:
      true
    else:
      let lcon = package.constraint.unsafeGet
      let rcon = by.constraint.unsafeGet
      let cmp = vercmp(lcon.version, rcon.version)
      checkConstraints(lcon.operation, rcon.operation, cmp)
  else:
    false

proc toPackageReference*(dependency: ptr AlpmDependency): PackageReference =
  let op = case dependency.depmod:
    of AlpmDepMod.eq: some(ConstraintOperation.eq)
    of AlpmDepMod.ge: some(ConstraintOperation.ge)
    of AlpmDepMod.le: some(ConstraintOperation.le)
    of AlpmDepMod.gt: some(ConstraintOperation.gt)
    of AlpmDepMod.lt: some(ConstraintOperation.lt)
    else: none(ConstraintOperation)

  let description = if dependency.desc != nil: some($dependency.desc) else: none(string)
  ($dependency.name, description, op.map(o => (o, $dependency.version, false)))

template toPackageReference*(pkg: ptr AlpmPackage): PackageReference =
  ($pkg.name, none(string), some((ConstraintOperation.eq, $pkg.version, false)))

template toPackageReference*(pkg: RpcPackageInfo): PackageReference =
  (pkg.name, none(string), some((ConstraintOperation.eq, pkg.version, false)))

proc parsePackageReference*(name: string, withDescription: bool): PackageReference =
  var matches: array[3, string]

  let descIndex = name.find(": ")
  let (description, workName) = if withDescription and descIndex >= 0:
      (some(name[descIndex + 2 .. ^1]), name[0 .. descIndex - 1])
    else:
      (none(string), name)

  if workName.match(re"([^><=]*)\ *(=|>=|<=|>|<)\ *([^ ]*)", matches):
    let constraints = toSeq(enumerate[ConstraintOperation]())
    let index = constraints.map(s => $s).find(matches[1])

    if index >= 0:
      (matches[0], description, some((constraints[index], matches[2], false)))
    else:
      (matches[0], description, none(VersionConstraint))
  else:
    (workName, description, none(VersionConstraint))

proc parseSrcInfoKeys(srcInfo: string):
  tuple[baseSeq: ref seq[SrcInfoPair], table: OrderedTable[string, ref seq[SrcInfoPair]]] =
  var table = initOrderedTable[string, ref seq[SrcInfoPair]]()
  var matches: array[2, string]
  var baseSeq: ref seq[SrcInfoPair]
  var values: ref seq[SrcInfoPair]

  new(baseSeq)
  baseSeq[] = newSeq[SrcInfoPair]()

  for line in srcInfo.splitLines:
    if line.match(re"[\t\ ]*(\w+)\ =\ (.*)", matches):
      let key = matches[0]
      let value = matches[1]

      if key == "pkgbase":
        values = baseSeq
      elif key == "pkgname":
        if table.hasKey(value):
          values = table[value]
        else:
          new(values)
          values[] = newSeq[SrcInfoPair]()
          table[value] = values

      if values != nil:
        values[] &= (key: key, value: value)

  (baseSeq: baseSeq, table: table)

proc parseSrcInfoName(repo: string, name: string, baseIndex: int, baseCount: int,
  rpcInfos: seq[RpcPackageInfo], baseSeq: ref seq[SrcInfoPair], nameSeq: ref seq[SrcInfoPair],
  arch: string, gitUrl: string, gitSubdir: Option[string]): Option[PackageInfo] =
  proc collectFromPairs(pairs: seq[SrcInfoPair], keyName: string): seq[string] =
    lc[x.value | (x <- pairs, x.key == keyName), string]

  proc collect(baseOnly: bool, keyName: string): seq[string] =
    let res = if baseOnly: @[] else: collectFromPairs(nameSeq[], keyName)
    if res.len == 0:
      collectFromPairs(baseSeq[], keyName).filter(x => x.len > 0)
    else:
      res.filter(x => x.len > 0)

  proc collectArch(baseOnly: bool, keyName: string): seq[PackageReference] =
    (collect(baseOnly, keyName) & collect(baseOnly, keyName & "_" & arch))
      .map(n => parsePackageReference(n, true))
      .filter(c => c.name.len > 0)

  proc filterReferences(references: seq[PackageReference],
    filterWith: seq[PackageReference]): seq[PackageReference] =
    references.filter(r => filterWith.filter(w => r.isProvidedBy(w, true)).len == 0)

  let base = lc[x.value | (x <- baseSeq[], x.key == "pkgbase"), string].optLast

  let version = collect(true, "pkgver").optLast
  let release = collect(true, "pkgrel").optLast
  let epoch = collect(true, "epoch").optLast
  let versionFull = lc[(v & "-" & r) | (v <- version, r <- release), string].optLast
    .map(v => epoch.map(e => e & ":" & v).get(v))

  let description = collect(false, "pkgdesc").optLast
  let archs = collect(false, "arch").filter(a => a != "any")
  let url = collect(false, "url").optLast
  let licenses = collect(false, "license")
  let groups = collect(false, "groups")
  let pgpKeys = collect(true, "validpgpkeys")

  let baseDepends = collectArch(true, "depends")
  let depends = collectArch(false, "depends")
  let makeDepends = (baseDepends & collectArch(true, "makedepends"))
    .deduplicate.filterReferences(depends)
  let checkDepends = collectArch(true, "checkdepends")
    .filterReferences(depends & makeDepends)
  let optional = collectArch(false, "optdepends")
  let provides = collectArch(false, "provides")
  let conflicts = collectArch(false, "conflicts")
  let replaces = collectArch(false, "replaces")

  let info = rpcInfos.filter(i => i.name == name).optLast

  lc[PackageInfo(baseIndex: baseIndex, baseCount: baseCount,
    repo: repo, base: b, name: name, version: v, description: description,
    archs: archs, url: url, licenses: licenses, groups: groups, pgpKeys: pgpKeys,
    depends: depends, makeDepends: makeDepends, checkDepends: checkDepends,
    optional: optional, provides: provides, conflicts: conflicts, replaces: replaces,
    maintainer: info.map(i => i.maintainer).flatten,
    firstSubmitted: info.map(i => i.firstSubmitted).flatten,
    lastModified: info.map(i => i.lastModified).flatten,
    outOfDate: info.map(i => i.outOfDate).flatten,
    votes: info.map(i => i.votes).get(0),
    popularity: info.map(i => i.popularity).get(0),
    gitUrl: gitUrl, gitSubdir: gitSubdir) |
    (b <- base, v <- versionFull), PackageInfo].optLast

proc parseSrcInfo*(repo: string, srcInfo: string, arch: string, gitUrl: string,
  gitSubdir: Option[string], rpcInfos: seq[RpcPackageInfo] = @[]): seq[PackageInfo] =
  let parsed = parseSrcInfoKeys(srcInfo)

  toSeq(parsed.table.namedPairs).foldl(a & toSeq(parseSrcInfoName(repo, b.key,
    a.len, parsed.table.len, rpcInfos, parsed.baseSeq, b.value, arch,
    gitUrl, gitSubdir).items), newSeq[PackageInfo]())

proc `$`*(reference: PackageReference): string =
  reference.constraint
    .map(c => reference.name & $c.operation & c.version)
    .get(reference.name)

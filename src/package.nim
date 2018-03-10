import
  future, options, os, re, sequtils, sets, strutils, tables,
  utils

type
  ConstraintOperation* {.pure.} = enum
    ge = ">=",
    gt = ">",
    eq = "=",
    lt = "<",
    le = "<="

  VersionConstraint* = tuple[
    operation: ConstraintOperation,
    version: string
  ]

  PackageReference* = tuple[
    name: string,
    description: Option[string],
    constraint: Option[VersionConstraint]
  ]

  ArchPackageReference* = tuple[
    arch: Option[string],
    reference: PackageReference
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
    votes*: int
    popularity*: float

  PackageInfo* = object of RpcPackageInfo
    archs*: seq[string]
    url*: Option[string]
    licenses*: seq[string]
    groups*: seq[string]
    depends*: seq[ArchPackageReference]
    makeDepends*: seq[ArchPackageReference]
    checkDepends*: seq[ArchPackageReference]
    optional*: seq[ArchPackageReference]
    provides*: seq[ArchPackageReference]
    conflicts*: seq[ArchPackageReference]
    replaces*: seq[ArchPackageReference]
    gitUrl*: string
    gitBranch*: Option[string]
    gitCommit*: Option[string]
    gitPath*: Option[string]

  GitRepo* = tuple[
    url: string,
    branch: string,
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
    (["arch"].toSet,
      ["core", "extra", "testing"].toSet,
      ("https://git.archlinux.org/svntogit/packages.git",
        "packages/${BASE}", "repos/${REPO}-${ARCH}")),
    (["arch"].toSet,
      ["community", "community-testing", "multilib", "multilib-testing"].toSet,
      ("https://git.archlinux.org/svntogit/community.git",
        "packages/${BASE}", "repos/${REPO}-${ARCH}"))
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

proc readOsId: string =
  var file: File
  if file.open("/usr/bin/os-release"):
    try:
      while true:
        let rawLine = readLine(file)
        if rawLine[0 .. 2] == "ID=":
          return rawLine[3 .. ^1]
    except EOFError:
      discard
    except IOError:
      discard
    finally:
      file.close()
  return "arch"

let osId = readOsId()

proc lookupGitRepo*(repo: string, base: string, arch: string): Option[GitRepo] =
  template replaceAll(gitPart: string): string =
    gitPart
      .replace("${REPO}", repo)
      .replace("${BASE}", base)
      .replace("${ARCH}", arch)

  packageRepos
    .filter(pr => osId in pr.os and repo in pr.repo)
    .map(pr => (pr.git.url.replaceAll, pr.git.branch.replaceAll, pr.git.path.replaceAll))
    .optFirst

template repoPath*(tmpRoot: string, base: string): string =
  tmpRoot & "/" & base

template buildPath*(repoPath: string, gitPath: Option[string]): string =
  gitPath.map(p => repoPath & "/" & p).get(repoPath)

template allDepends*(pkgInfo: PackageInfo): seq[ArchPackageReference] =
  pkgInfo.depends & pkgInfo.makeDepends & pkgInfo.checkDepends

proc parseSrcInfoKeys(srcInfo: string):
  tuple[baseSeq: ref seq[SrcInfoPair], table: Table[string, ref seq[SrcInfoPair]]] =
  var table = initTable[string, ref seq[SrcInfoPair]]()
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

proc parseSrcInfoName(repo: string, name: string, rpcInfos: seq[RpcPackageInfo],
  baseSeq: ref seq[SrcInfoPair], nameSeq: ref seq[SrcInfoPair],
  gitUrl: string, gitBranch: Option[string], gitCommit: Option[string],
  gitPath: Option[string]): Option[PackageInfo] =
  let pairs = baseSeq[] & nameSeq[]
  proc collect(keyName: string): seq[string] =
    lc[x.value | (x <- pairs, x.key == keyName), string]

  proc splitConstraint(name: string): PackageReference =
    var matches: array[3, string]

    let descIndex = name.find(": ")
    let (description, workName) = if descIndex >= 0:
        (some(name[descIndex + 2 .. ^1]), name[0 .. descIndex - 1])
      else:
        (none(string), name)

    if workName.match(re"([^><=]*)\ *(>|<|=|>=|<=)\ *([^ ]*)", matches):
      let constraints = toSeq(enumerate[ConstraintOperation]())
      let index = constraints.map(s => $s).find(matches[1])

      if index >= 0:
        (matches[0], description, some((constraints[index], matches[2])))
      else:
        (matches[0], description, none(VersionConstraint))
    else:
      (workName, description, none(VersionConstraint))

  proc collectArch(keyName: string, arch: Option[string]): seq[ArchPackageReference] =
    collect(arch.map(a => keyName & "_" & a).get(keyName))
      .map(splitConstraint)
      .map(c => (arch, (c.name, c.description, c.constraint)))

  proc collectArchs(keyName: string, archs: seq[string]): seq[ArchPackageReference] =
    let archsFull = none(string) & archs.map(some)
    lc[x | (a <- archsFull, x <- collectArch(keyName, a)), ArchPackageReference]

  let base = lc[x.value | (x <- baseSeq[], x.key == "pkgbase"), string].optLast

  let version = collect("pkgver").optLast
  let release = collect("pkgrel").optLast
  let epoch = collect("epoch").optLast
  let versionFull = lc[(v & "-" & r) | (v <- version, r <- release), string].optLast
    .map(v => epoch.map(e => e & ":" & v).get(v))

  let description = collect("pkgdesc").optLast
  let archs = collect("arch").filter(a => a != "any")
  let url = collect("url").optLast
  let licenses = collect("license")
  let groups = collect("groups")

  let depends = collectArchs("depends", archs)
  let makeDepends = collectArchs("makedepends", archs)
  let checkDepends = collectArchs("checkdepends", archs)
  let optional = collectArchs("optdepends", archs)
  let provides = collectArchs("provides", archs)
  let conflicts = collectArchs("conflicts", archs)
  let replaces = collectArchs("replaces", archs)

  let info = rpcInfos.filter(i => i.name == name).optLast

  lc[PackageInfo(repo: repo, base: b, name: name, version: v, description: description,
    archs: archs, url: url, licenses: licenses, groups: groups,
    depends: depends, makeDepends: makeDepends, checkdepends: checkDepends,
    optional: optional, provides: provides, conflicts: conflicts, replaces: replaces,
    maintainer: info.map(i => i.maintainer).flatten,
    firstSubmitted: info.map( i => i.firstSubmitted).flatten,
    lastModified: info.map( i => i.lastModified).flatten,
    votes: info.map(i => i.votes).get(0),
    popularity: info.map(i => i.popularity).get(0),
    gitUrl: gitUrl, gitBranch: gitBranch, gitCommit: gitCommit, gitPath: gitPath) |
    (b <- base, v <- versionFull), PackageInfo].optLast

proc parseSrcInfo*(repo: string, srcInfo: string,
  gitUrl: string, gitBranch: Option[string], gitCommit: Option[string],
  gitPath: Option[string], rpcInfos: seq[RpcPackageInfo] = @[]): seq[PackageInfo] =
  let parsed = parseSrcInfoKeys(srcInfo)
  let packageSeq = toSeq(parsed.table.namedPairs)
  lc[x | (pair <- packageSeq, x <- parseSrcInfoName(repo, pair.key, rpcInfos,
    parsed.baseSeq, pair.value, gitUrl, gitBranch, gitCommit, gitPath)), PackageInfo]

proc `$`*(reference: PackageReference): string =
  reference.constraint
    .map(c => reference.name & $c.operation & c.version)
    .get(reference.name)

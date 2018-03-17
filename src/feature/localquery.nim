import
  algorithm, future, options, sequtils, sets, strutils, tables,
  "../args", "../config", "../format", "../package", "../pacman", "../utils",
  "../wrapper/alpm"

proc handleQueryOrphans*(args: seq[Argument], config: Config): int =
  type Package = tuple[name: string, explicit: bool]

  let (installed, alternatives) = withAlpm(config.root, config.db,
    newSeq[string](), config.arch, handle, dbs, errors):
    for e in errors: printError(config.color, e)

    var installed = initTable[Package, HashSet[PackageReference]]()
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

      installed.add(($pkg.name, pkg.reason == AlpmReason.explicit),
        depends + optional)
      if provides.len > 0:
        alternatives.add($pkg.name, provides)

    (installed, alternatives)

  let providedBy = lc[(y, x.key) | (x <- alternatives.namedPairs, y <- x.value),
    tuple[reference: PackageReference, name: string]]

  let installedSeq = lc[x | (x <- installed.pairs),
    tuple[package: Package, dependencies: HashSet[PackageReference]]]
  let explicit = installedSeq
    .filter(t => t.package.explicit)
    .map(t => t.package.name)
    .toSet

  proc findRequired(results: HashSet[string], check: HashSet[string]): HashSet[string] =
    let full = results + check

    let direct = lc[x | (y <- installedSeq, y.package.name in check,
      x <- y.dependencies), PackageReference]

    let indirect = lc[x.name | (y <- direct, x <- providedBy,
      y.isProvidedBy(x.reference)), string].toSet

    let checkNext = (direct.map(p => p.name).toSet + indirect) - full
    if checkNext.len > 0: findRequired(full, checkNext) else: full

  let required = findRequired(initSet[string](), explicit)
  let orphans = installedSeq.map(t => t.package.name).toSet - required

  let targets = args.targets.map(t => (if t[0 .. 5] == "local/": t[6 .. ^1] else: t))

  # Provide similar output for not installed packages
  let unknownTargets = targets.toSet - toSeq(installed.keys).map(p => p.name).toSet
  let results = if targets.len > 0:
      targets.filter(t => t in orphans or t in unknownTargets)
    else:
      toSeq(orphans.items).sorted(cmp)

  if results.len > 0:
    let newArgs = args.filter(arg => not arg.isTarget and
      not arg.matchOption((some("t"), "unrequired")) and
      not arg.matchOption((some("d"), "deps"))) &
      results.map(r => (r, none(string), ArgumentType.target))
    pacmanExec(false, config.color, newArgs)
  elif targets.len == 0:
    0
  else:
    1

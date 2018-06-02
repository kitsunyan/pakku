import
  algorithm, future, options, sequtils, sets, strutils, tables,
  "../args", "../common", "../config", "../format", "../package", "../pacman", "../utils",
  "../wrapper/alpm"

proc handleQueryOrphans*(args: seq[Argument], config: Config): int =
  let (installed, orphans, _, alternatives) = withAlpmConfig(config, false, handle, dbs, errors):
    for e in errors: printError(config.color, e)
    queryUnrequired(handle, true, false, initSet[string]())

  let targets = args.packageTargets(false)

  proc isOrphanOrNotFound(reference: PackageReference): bool =
    for r in installed:
      if reference.isProvidedBy(r, true):
        return reference.name in orphans
    for name, references in alternatives:
      for r in references:
        let hasConstraint = r.constraint.isSome and not r.constraint.unsafeGet.impliedVersion
        if (not hasConstraint and reference.constraint.isNone and r.name == reference.name) or
          (hasConstraint and reference.isProvidedBy(r, true)):
          return name in orphans
    return true

  # Provide similar output for not installed packages
  let results = if targets.len > 0:
      targets.filter(t => (t.repo.isNone or t.repo == some("local")) and
        t.reference.isOrphanOrNotFound).map(t => $t)
    else:
      toSeq(orphans.items).sorted(cmp)

  if results.len > 0:
    let newArgs = args.filter(arg => not arg.isTarget and
      not arg.matchOption(%%%"unrequired") and
      not arg.matchOption(%%%"deps")) &
      results.map(r => (r, none(string), ArgumentType.target))
    pacmanExec(false, config.color, newArgs)
  elif targets.len == 0:
    0
  else:
    1

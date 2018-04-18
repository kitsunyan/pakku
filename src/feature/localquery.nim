import
  algorithm, future, options, sequtils, sets, strutils, tables,
  "../args", "../common", "../config", "../format", "../package", "../pacman", "../utils",
  "../wrapper/alpm"

proc handleQueryOrphans*(args: seq[Argument], config: Config): int =
  let (installed, orphans, _) = withAlpm(config.root, config.db, newSeq[string](),
    config.arch, handle, dbs, errors):
    for e in errors: printError(config.color, e)
    queryUnrequired(handle, true, false, initSet[string]())

  let targets = args.targets.map(t => (if t[0 .. 5] == "local/": t[6 .. ^1] else: t))

  # Provide similar output for not installed packages
  let unknownTargets = targets.toSet - installed
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

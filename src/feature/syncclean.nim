import
  options, os, posix, strutils,
  "../args", "../common", "../config", "../format", "../pacman", "../utils",
  "../wrapper/alpm"

proc handleSyncClean*(args: seq[Argument], config: Config): int =
  let code = pacmanRun(true, config.color, args)
  if code != 0:
    code
  else:
    let cleanCount = args.count(%%%"clean")
    let noconfirm = args.noconfirm

    let reposCacheDir = config.userCacheCurrent.cache(CacheKind.repositories)
    let homeDir = currentUser.home
    let printReposCacheDir = if reposCacheDir.find(homeDir & "/") == 0:
        '~' & reposCacheDir[homeDir.len .. ^1]
      else:
        reposCacheDir
    echo()
    echo(tr"Repositories directory: $#" % [printReposCacheDir])

    if (cleanCount == 1 and
      printColonUserChoice(config.color, tr"Do you want to remove unused repositories?",
      ['y', 'n'], 'y', 'n', noconfirm, 'y') == 'y') or
      (cleanCount >= 2 and
      printColonUserChoice(config.color, tr"Do you want to remove ALL repositories?",
      ['y', 'n'], 'n', 'n', noconfirm, 'n') == 'y'):
      if cleanCount == 1:
        echo(tr"removing unused package repositories...")
      else:
        echo(tr"removing all package repositories...")

      if existsDir(reposCacheDir):
        withAlpmConfig(config, false, handle, dbs, errors):
          for e in errors: printError(config.color, e)

          let local = handle.local
          for file in walkDir(reposCacheDir):
            if file.kind == pcDir:
              let index = file.path.rfind('/')
              let dirName = if index >= 0: file.path[index + 1 .. ^1] else: file.path
              for name in bareFullNameDeconstruct(BareKind.pkg, dirName):
                if cleanCount >= 2 or local[name] == nil:
                  removeDirQuiet(file.path)

      discard rmdir(reposCacheDir)
      discard rmdir(config.userCacheCurrent)

    0

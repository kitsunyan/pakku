import future, os, posix, sequtils, strutils

proc splitCommands(params: seq[string], index: int, res: seq[seq[string]]): seq[seq[string]] =
  if index < params.len:
    let count = params[index].parseInt
    let args = params[index + 1 .. index + count]
    splitCommands(params, index + count + 1, res & args)
  else:
    res

proc perror*(s: cstring): void {.importc, header: "<stdio.h>".}
template perror*: void = perror(paramStr(0))

proc runCommand(params: seq[string], inputOutput: bool): int =
  if params.len > 0:
    let pid = fork()
    if pid == 0:
      if not inputOutput:
        discard close(0)
        discard close(1)
        discard open("/dev/null")
        discard open("/dev/null")
      let cexec = allocCStringArray(params)
      let code = execvp(cexec[0], cexec)
      perror()
      deallocCStringArray(cexec)
      quit(code)
    else:
      var status: cint = 1
      discard waitpid(pid, status, 0)
      if WIFEXITED(status):
        WEXITSTATUS(status)
      else:
        discard kill(getpid(), status)
        1
  else:
    0

proc take(params: seq[string], start: int, count: int): (seq[string], int) =
  let res = params[start .. start + count - 1]
  (res, start + count)

proc handleInstall*(params: seq[string]): int =
  let (head, headIndex) = params.take(0, 3)
  let destination = head[0]
  let uid = head[1].parseInt
  let gid = head[2].parseInt

  let (upgradeCommand, upgradeIndex) = params
    .take(headIndex + 1, params[headIndex].parseInt)
  let (databaseCommand, databaseIndex) = params
    .take(upgradeIndex + 1, params[upgradeIndex].parseInt)

  let packages = params[databaseIndex .. ^1]
  if packages.len /% 3 * 3 != packages.len:
    raise newException(SystemError, "invalid arguments")

  let install: seq[tuple[name: string, file: string, mode: string]] = packages
    .distribute(packages.len /% 3)
    .map(l => (l[0], l[1], l[2]))

  if uid >= 0 and gid >= 0:
    for pkg in install:
      try:
        let index = pkg.file.rfind("/")
        let name = if index >= 0: pkg.file[index + 1 .. ^1] else: pkg.file
        let dest = destination & "/" & name
        copyFile(pkg.file, dest)
        discard chown(dest, (Uid) uid, (Gid) gid)
      except:
        discard

  let asexplicit = install.filter(p => p.mode == "explicit").map(p => p.name)
  let asdeps = install.filter(p => p.mode == "dependency").map(p => p.name)

  let installCode = runCommand(upgradeCommand & "--" & install.map(p => p.file), true)

  let asexplicitCode = if asexplicit.len > 0:
      runCommand(databaseCommand & "--asexplicit" & "--" & asexplicit, false)
    else:
      0

  let asdepsCode = if asdeps.len > 0:
      runCommand(databaseCommand & "--asdeps" & "--" & asdeps, false)
    else:
      0

  if installCode != 0:
    installCode
  elif asexplicitCode != 0:
    asexplicitCode
  else:
    asdepsCode

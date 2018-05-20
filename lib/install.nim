import future, os, posix, sequtils, strutils

let params = commandLineParams()
let destination = params[0]
let uid = params[1].parseInt
let gid = params[2].parseInt
let paramsStart = 3

proc splitCommands(index: int, res: seq[seq[string]]): seq[seq[string]] =
  if index < params.len:
    let count = params[index].parseInt
    let args = params[index + 1 .. index + count]
    splitCommands(index + count + 1, res & args)
  else:
    res

let commands = splitCommands(paramsStart, @[])
let targets = lc[x | (y <- commands[1 .. ^1], x <- y), string]

if uid >= 0 and gid >= 0:
  for file in targets:
    try:
      let index = file.rfind("/")
      let name = if index >= 0: file[index + 1 .. ^1] else: file
      let dest = destination & "/" & name
      copyFile(file, dest)
      discard chown(dest, (Uid) uid, (Gid) gid)
    except:
      discard

proc perror*(s: cstring): void {.importc, header: "<stdio.h>".}
template perror*: void = perror(getAppFilename())

proc runCommand(params: seq[string]): int =
  if params.len > 0:
    let pid = fork()
    if pid == 0:
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

proc buildParams(index: int, asArg: string): seq[string] =
  if commands[index].len > 0: commands[0] & asArg & "--" & commands[index] else: @[]

let asdepsCode = runCommand(buildParams(1, "--asdeps"))
if asdepsCode == 0:
  programResult = runCommand(buildParams(2, "--asexplicit"))
else:
  programResult = asdepsCode

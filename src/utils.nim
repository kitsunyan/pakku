import
  future, hashes, options, os, osproc, posix, strutils, tables

type
  HaltError* = object of Exception
    code*: int

  CommandError* = object of Exception
    color*: Option[bool]
    error*: bool

const
  pkgLibDir* = getenv("PROG_PKGLIBDIR")
  localStateDir* = getenv("PROG_LOCALSTATEDIR")
  sysConfDir* = getenv("PROG_SYSCONFDIR")

  bashCmd* = "/bin/bash"
  suCmd* = "/usr/bin/su"
  sudoCmd* = "/usr/bin/sudo"
  gitCmd* = "/usr/bin/git"
  pacmanCmd* = "/usr/bin/pacman"
  makepkgCmd* = "/usr/bin/makepkg"

template haltError*(code: int): untyped =
  var e: ref HaltError
  new(e)
  e.code = code
  e

template commandError*(message: string, colorNeeded: Option[bool] = none(bool),
  showError: bool = true): untyped =
  var e: ref CommandError
  new(e)
  e.msg = message
  e.color = colorNeeded
  e.error = showError
  e

iterator items*[T](self: Option[T]): T {.raises: [].} =
  if self.isSome:
    yield self.unsafeGet

template len*[T](self: Option[T]): int =
  if self.isSome: 1 else: 0

template orElse*[T, R](opt1: Option[T], opt2: Option[R]): Option[R] =
  if opt1.isSome: opt1 else: opt2

template hash*[T](opt: Option[T]): int =
  opt.map(hash).get(0)

proc opt*[K, V](table: Table[K, V], key: K): Option[V] =
  if table.hasKey(key): some(table[key]) else: none(V)

proc opt*[K, V](table: OrderedTable[K, V], key: K): Option[V] =
  if table.hasKey(key): some(table[key]) else: none(V)

proc optFirst*[T](s: openArray[T]): Option[T] =
  if s.len > 0: some(s[s.low]) else: none(T)

proc optLast*[T](s: openArray[T]): Option[T] =
  if s.len > 0: some(s[s.high]) else: none(T)

iterator enumerate*[T: enum]: T =
  let elow = T.low.ord
  let ehigh = T.high.ord
  for i in elow .. ehigh:
    yield T(i)

iterator namedPairs*[K, V](table: Table[K, V]): tuple[key: K, value: V] =
  for key, value in table.pairs:
    yield (key, value)

iterator reversed*[T](s: openArray[T]): T =
    for i in countdown(s.len - 1, 0):
      yield s[i]

proc groupBy*[T, X](s: seq[T], callback: T -> X): seq[tuple[key: X, values: seq[T]]] =
  var table = initOrderedTable[X, ref seq[T]]()
  for value in s:
    let key = callback(value)
    var work: ref seq[T]
    if table.hasKey(key):
      work = table[key]
    else:
      new(work)
      work[] = newSeq[T]()
      table[key] = work
    work[] &= value

  result = newSeq[tuple[key: X, values: seq[T]]]()
  for key, values in table.pairs:
    result &= (key, values[])

proc perror*(s: cstring): void {.importc, header: "<stdio.h>".}
template perror*: void = perror(getAppFilename())

proc execResult*(args: varargs[string]): int =
  let cexec = allocCStringArray(args)
  let code = execvp(cexec[0], cexec)
  perror()
  deallocCStringArray(cexec)
  code

const
  interruptSignals* = [SIGINT, SIGTERM]

template blockSignals*(signals: openArray[cint],
  unblock: untyped, body: untyped): untyped =
  block:
    var sigset: Sigset
    var sigoldset: Sigset

    discard sigemptyset(sigset)
    for s in signals:
      discard sigaddset(sigset, s)
    discard sigprocmask(SIG_BLOCK, sigset, sigoldset)

    var unblocked = false
    let unblock = () => (block:
      if not unblocked:
        discard sigprocmask(SIG_SETMASK, sigoldset, sigset)
      unblocked = true)

    try:
      body
    finally:
      unblock()

proc forkWait*(call: () -> int): int =
  blockSignals(interruptSignals, unblock):
    let pid = fork()
    if pid == 0:
      unblock()
      quit(call())
    else:
      var status: cint = 1
      discard waitpid(pid, status, 0)
      if WIFEXITED(status):
        return WEXITSTATUS(status)
      else:
        discard kill(getpid(), status)
        return 1

proc runProgram*(args: varargs[string]): seq[string] =
  let output = execProcess(args[0], @args[1 .. ^1], options = {})
  if output.len == 0:
    @[]
  elif output.len > 0 and $output[^1] == "\n":
    output[0 .. ^2].split("\n")
  else:
    output.split("\n")

proc setenv*(name: cstring, value: cstring, override: cint): cint
  {.importc, header: "<stdlib.h>".}

proc getUser*: (int, string) =
  let uid = getuid()
  while true:
    var pw = getpwent()
    if pw == nil:
      raise newException(SystemError, "")
    if pw.pw_uid == uid:
      return (uid.int, $pw.pw_name)

proc toString*[T](arr: array[T, char], length: Option[int]): string =
  var workLength = length.get(T.high + 1)
  var str = newStringOfCap(workLength)
  for i in 0 .. workLength - 1:
    let c = arr[i]
    if length.isNone and c == '\0':
      break
    str.add(arr[i])
  str

proc removeDirQuiet*(s: string) =
  try:
    removeDir(s)
  except:
    discard

proc dgettext(domain: cstring, s: cstring): cstring
  {.cdecl, importc: "dgettext".}

proc gettext(domain: string, s: string): string =
  let translated = dgettext(domain, s)
  if translated != nil: $translated else: s

proc gettextHandle(domain: string, s: string): string =
  let res = gettext(domain, s).replace("%s", "$#").replace("%c", "$#")
  if res.len > 0 and res[^1 .. ^1] == "\n": res[0 .. ^2] else: res

template tr*(s: string): string =
  gettext("pakku", s)

template trp*(s: string): string =
  gettextHandle("pacman", s)

template tra*(s: string): string =
  gettextHandle("libalpm", s)

template trc*(s: string): string =
  gettextHandle("libc", s)

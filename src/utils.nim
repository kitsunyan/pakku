import
  hashes, options, os, posix, sequtils, strutils, sugar, tables

type
  HaltError* = object of Exception
    code*: int

  CommandError* = object of Exception
    color*: Option[bool]
    error*: bool

  User* = tuple[
    name: string,
    uid: int,
    gid: int,
    groups: seq[int],
    home: string,
    shell: string
  ]

proc cgetenv*(name: cstring): cstring
  {.importc: "getenv", header: "<stdlib.h>".}

proc csetenv*(name: cstring, value: cstring, override: cint): cint
  {.importc: "setenv", header: "<stdlib.h>".}

proc cunsetenv*(name: cstring): cint
  {.importc: "unsetenv", header: "<stdlib.h>".}

const
  pkgLibDir* = getEnv("PROG_PKGLIBDIR")
  localStateDir* = getEnv("PROG_LOCALSTATEDIR")
  sysConfDir* = getEnv("PROG_SYSCONFDIR")

  bashCmd* = "/bin/bash"
  suCmd* = "/usr/bin/su"
  sudoCmd* = "/usr/bin/sudo"
  gitCmd* = "/usr/bin/git"
  gpgCmd* = "/usr/bin/gpg"
  gpgConfCmd* = "/usr/bin/gpgconf"
  pacmanCmd* = "/usr/bin/pacman"
  makepkgCmd* = "/usr/bin/makepkg"

template haltError*(exitCode: int): untyped =
  var e: ref HaltError
  new(e)
  e.code = exitCode
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

template namedPairsTyped(T: typedesc) =
  iterator namedPairs*[K, V](table: T[K, V]): tuple[key: K, value: V] =
    for key, value in table.pairs:
      yield (key, value)

namedPairsTyped(Table)
namedPairsTyped(OrderedTable)

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

let
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

proc forkWaitInternal(call: () -> int, beforeWait: () -> void): int =
  blockSignals(interruptSignals, unblock):
    let pid = fork()
    if pid == 0:
      unblock()
      quit(call())
    else:
      beforeWait()
      var status: cint = 1
      discard waitpid(pid, status, 0)
      if WIFEXITED(status):
        return WEXITSTATUS(status)
      else:
        discard kill(getpid(), status)
        return 1

proc forkWait*(call: () -> int): int =
  forkWaitInternal(call, proc = discard)

proc forkWaitRedirect*(call: () -> int): tuple[output: seq[string], code: int] =
  var fd: array[2, cint]
  discard pipe(fd)

  var data = newSeq[char]()

  let code = forkWaitInternal(() => (block:
    discard close(fd[0])
    discard close(1)
    discard dup(fd[1])
    discard close(fd[1])
    discard close(0)
    discard open("/dev/null")
    discard close(2)
    discard open("/dev/null")
    call()), () => (block:
    discard close(fd[1])
    var buffer: array[80, char]
    while true:
      let count = read(fd[0], addr(buffer[0]), buffer.len)
      if count <= 0:
        break
      data &= buffer[0 .. count - 1]
    discard close(fd[0])))

  var output = newStringOfCap(data.len)
  for c in data:
    output &= c

  let lines = if output.len == 0:
      @[]
    elif output.len > 0 and $output[^1] == "\n":
      output[0 .. ^2].split("\n")
    else:
      output.split("\n")

  (lines, code)

proc getgrouplist*(user: cstring, group: Gid, groups: ptr cint, ngroups: var cint): cint
  {.importc, header: "<grp.h>".}

proc setgroups*(size: csize, groups: ptr cint): cint
  {.importc, header: "<grp.h>".}

proc getUser(uid: int): User =
  var pw = getpwuid(Uid(uid))
  if pw == nil:
    raise newException(CatchableError, "")
  var groups: array[100, cint]
  var ngroups: cint = 100
  if getgrouplist(pw.pw_name, pw.pw_gid, addr(groups[0]), ngroups) < 0:
    raise newException(CatchableError, "")
  else:
    let groupsSeq = groups[0 .. ngroups - 1].map(x => x.int)
    let res = ($pw.pw_name, pw.pw_uid.int, pw.pw_gid.int, groupsSeq,
      $pw.pw_dir, $pw.pw_shell)
    return res

let currentUser* = getUser(getuid().int)

let initialUser* = try:
  let sudoUid = getEnv("SUDO_UID")
  let polkitUid = getEnv("PKEXEC_UID")

  let uidString = if sudoUid.len > 0:
      some(sudoUid)
    elif polkitUid.len > 0:
      some(polkitUid)
    else:
      none(string)

  let uid = uidString.get.parseInt
  if uid == 0 or currentUser.uid != 0: none(User) else: some(getUser(uid))
except:
  none(User)

proc canDropPrivileges*(): bool =
  initialUser.isSome

proc dropPrivileges*(): bool =
  if initialUser.isSome:
    let user = initialUser.unsafeGet
    var groups = user.groups.map(x => x.cint)

    if setgroups(user.groups.len, addr(groups[0])) < 0:
      return false
    if setgid((Gid) user.gid) != 0:
      return false
    if setuid((Uid) user.uid) != 0:
      return false

    template replaceExisting(name: string, value: string) =
      if cgetenv(name) != nil:
        discard csetenv(name, value, 1)

    replaceExisting("USER", user.name)
    replaceExisting("USERNAME", user.name)
    replaceExisting("LOGNAME", user.name)
    replaceExisting("HOME", user.home)
    replaceExisting("SHELL", user.shell)

    discard cunsetenv("SUDO_COMMAND")
    discard cunsetenv("SUDO_USER")
    discard cunsetenv("SUDO_UID")
    discard cunsetenv("SUDO_GID")
    discard cunsetenv("PKEXEC_UID")

    return true
  else:
    return true

proc checkExec(file: string): bool =
  var statv: Stat
  stat(file, statv) == 0 and (statv.st_mode.cint and S_IXUSR) == S_IXUSR

let sudoPrefix*: seq[string] = if checkExec(sudoCmd):
    @[sudoCmd]
  elif checkExec(suCmd):
    @[suCmd, "root", "-c", "exec \"$@\"", "--", "sh"]
  else:
    @[]

var intSigact: SigAction
intSigact.sa_handler = SIG_DFL
discard sigaction(SIGINT, intSigact)

var wasInterrupted = false

proc interruptHandler(signal: cint): void {.noconv.} =
  wasInterrupted = true

template catchInterrupt*(body: untyped): untyped =
  block:
    var intSigact: SigAction
    var oldIntSigact: SigAction
    intSigact.sa_handler = interruptHandler
    discard sigaction(SIGINT, intSigact, oldIntSigact)
    let data = body
    let interrupted = wasInterrupted
    wasInterrupted = false
    discard sigaction(SIGINT, oldIntSigact)
    (data, interrupted)

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

const bashSpecialCharacters = " \t\"'`()[]{}#&|;!\\*~<>?"

proc bashEscape*(s: string): string =
  result = ""
  for c in s:
    if c in bashSpecialCharacters:
      result &= "\\" & c
    elif c == "\n"[0]:
      result &= "$'\\n'"
    elif c.cuint < 0x20.cuint or c.cuint > 0x80.cuint:
      result &= "$'\\0x" & c.uint8.toHex & "'"
    else:
      result &= c

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

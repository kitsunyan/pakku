import
  macros, options, posix, sequtils, strutils, sugar, times, unicode,
  utils

type
  PackageLineFormat* = tuple[
    title: string,
    values: seq[string],
    forceBreak: bool
  ]

  PackageInstallFormat* = tuple[
    name: string,
    repo: string,
    oldVersion: Option[string],
    newVersion: string
  ]

  CommentFormat* = tuple[
    author: string,
    date: string,
    text: string
  ]

  Color {.pure.} = enum
    normal = "\x1b[0m"
    red = "\x1b[1;31m"
    green = "\x1b[1;32m"
    yellow = "\x1b[1;33m"
    blue = "\x1b[1;34m"
    magenta = "\x1b[1;35m"
    cyan = "\x1b[1;36m"
    bold = "\x1b[1;39m"

template `^`(c: Color): string =
  if color: $c else: ""

type
  WinSize = object
    row: cushort
    col: cushort
    xpixel: cushort
    ypixel: cushort

proc ioctl[T](fd: cint, request: culong, argp: var T): cint
  {.importc, header: "<sys/ioctl.h>".}

proc getWindowSize(): tuple[width: int, height: int] =
  var winSize: WinSize
  if ioctl(1, 0x5413, winSize) != -1:
    ((int) winSize.col, (int) winSize.row)
  else:
    (0, 0)

proc tcflush(fd: cint, queueSelector: cint): cint
  {.importc, header: "<termios.h>".}

proc discardStream() =
  if isatty(0) != 0:
    discard tcflush(0, 0)

proc printError*(color: bool, s: string) =
  stderr.writeLine(^Color.red, trp"error: ", ^Color.normal, s)

proc printWarning*(color: bool, s: string) =
  stderr.writeLine(^Color.yellow, trp"warning: ", ^Color.normal, s)

proc printFile*(color: bool, quiet: bool, name: string, file: string) =
  if quiet:
    echo(file)
  else:
    echo(^Color.bold, name, ^Color.normal, ' ', file)

proc formatPkgRating*(votes: int, popularity: float): string =
  $votes & " / " & formatFloat(popularity, format = ffDecimal, precision = 6)

proc computeMaxLength*(texts: openArray[string]): int =
  texts.map(runeLen).max

proc splitLines(text: string, lineSize: int, lines: seq[string] = @[]): seq[string] =
  let addBreaks = lineSize >= 10

  if not addBreaks:
    lines & text
  else:
    let offset = text.runeOffset(lineSize)
    if offset < 0:
      lines & text
    else:
      let leftIndex = text.rfind(' ', offset - 1)
      let rightIndex = text.find(' ', offset - 1)
      let index = if leftIndex >= 0: leftIndex else: rightIndex

      if index < 0:
        lines & text
      else:
        strutils.strip(text[index .. ^1]).splitLines(lineSize,
          lines & strutils.strip(text[0 .. index - 1]))

proc printPackageInfo*(minPadding: int, color: bool, lines: varargs[PackageLineFormat]) =
  let width = getWindowSize().width
  let divider = " : "
  let padding = max(lines.map(line => line.title.runeLen).max, minPadding)

  let lineSize = width - (padding + divider.len)

  proc formatTextLines(values: seq[string], forceBreak: bool): seq[string] =
    if values.len == 0:
      @[]
    elif forceBreak:
      lc[x | (y <- values.map(s => strutils.strip(s).splitLines(lineSize)), x <- y), string]
    else:
      values.map(v => strutils.strip(v)).foldl(a & "  " & b).splitLines(lineSize)

  proc formatText(values: seq[string], forceBreak: bool): string =
    let textSeq = formatTextLines(values, forceBreak)
    if textSeq.len > 0:
      textSeq.foldl(a & "\n" & ' '.repeat(padding + divider.len) & b)
    else:
      "None"

  for line in lines:
    let title = line.title & ' '.repeat(padding - line.title.runeLen) & divider
    let text = formatText(line.values, line.forceBreak)
    echo(^Color.bold, title, ^Color.normal, text)

  # pacman leaves empty line in the end of info
  echo()

proc printPackageSearch*(color: bool, repo: string, name: string,
  version: string, installedVersion: Option[string],
  description: Option[string], extra: Option[string]) =
  let commonText = ^Color.magenta & repo & "/" &
    ^Color.bold & name & " " & ^Color.green & version & ^Color.normal

  let installedText = if installedVersion == some(version):
      " " & ^Color.cyan &
        "[" & trp"installed" & "]" & ^Color.normal
    elif installedVersion.isSome:
      " " & ^Color.cyan &
        "[" & trp"installed" & ": " & installedVersion.unsafeGet &
        "]" & ^Color.normal
    else:
      ""

  let extraText = extra.map(e => " " & ^Color.yellow &
    "[" & e & "]" & ^Color.normal).get("")

  echo(commonText & installedText & extraText)

  let padding = 4
  let lines = description.get("").splitLines(getWindowSize().width - padding)
  for line in lines:
    echo(' '.repeat(padding), line)

proc printPackagesRegular(color: bool, warn: bool, packages: seq[PackageInstallFormat]) =
  let title = trp"Packages" & " (" & $packages.len & ") "
  let padding = title.runeLen
  let lines = packages.map(p => p.name & "-" & p.newVersion).foldl(a & "  " & b)
    .splitLines(getWindowSize().width - padding)

  echo()
  if warn:
    printWarning(color, trp("insufficient columns available for table display\n"))
  echo(^Color.bold, title, ^Color.normal, lines[0])
  for line in lines[1 .. ^1]:
    echo(' '.repeat(padding), line)
  echo()

proc printPackagesVerbose(color: bool, packages: seq[PackageInstallFormat]) =
  let packageTitle = trp"Package" & " (" & $packages.len & ")"
  let oldVersionTitle = trp"Old Version"
  let newVersionTitle = trp"New Version"

  let packageLen = max(packageTitle.runeLen,
    packages.map(p => p.name.len + 1 + p.repo.len).max)

  let oldVersionLenEmpty = packages.map(p => p.oldVersion.map(v => v.len).get(0)).max
  let oldVersionLen = if oldVersionLenEmpty > 0:
      max(oldVersionTitle.runeLen, oldVersionLenEmpty)
    else:
      0

  let newVersionLenEmpty = packages.map(p => p.newVersion.len).max
  let newVersionLen = if newVersionLenEmpty > 0:
      max(newVersionTitle.runeLen, newVersionLenEmpty)
    else:
      0

  let totalLen = packageLen +
    (if oldVersionLen > 0: oldVersionLen + 2 else: 0) +
    (if newVersionLen > 0: newVersionLen + 2 else: 0)

  let width = getWindowSize().width
  if width > 0 and totalLen > width:
    printPackagesRegular(color, true, packages)
  else:
    echo()
    echo(^Color.bold & packageTitle &
      ' '.repeat(packageLen - packageTitle.runeLen) &
      (if oldVersionLen > 0: "  " & oldVersionTitle &
        ' '.repeat(oldVersionLen - oldVersionTitle.runeLen) else: "") &
      "  " & newVersionTitle & ^Color.normal)
    echo()
    for package in packages:
      let name = package.repo & "/" & package.name
      let oldVersion = package.oldVersion.get("")
      echo(name & ' '.repeat(packageLen - name.runeLen) &
        (if oldVersionLen > 0: "  " & oldVersion &
          ' '.repeat(oldVersionLen - oldVersion.len) else: "") &
        "  " & package.newVersion)
    echo()

proc printPackages*(color: bool, verbose: bool, packages: seq[PackageInstallFormat]) =
  if verbose:
    printPackagesVerbose(color, packages)
  else:
    printPackagesRegular(color, false, packages)

proc printComments*(color: bool, maintainer: Option[string],
  comments: seq[CommentFormat]) =
  echo()
  for comment in comments:
    let badge = if maintainer == some(comment.author):
        ^Color.cyan & "[maintainer]" & ^Color.normal & " "
      else:
        ""
    echo(^Color.blue & comment.author & ^Color.normal & " " & badge &
      ^Color.bold & comment.date & ^Color.normal)
    echo(comment.text.replace("\n\n", "\n"))
    echo()

proc printColon*(color: bool, s: string) =
  echo(^Color.blue, ":: ", ^Color.bold, s, ^Color.normal)

proc printColonUserInput*(color: bool, s: string,
  noconfirm: bool, default: string, cancel: string): string =
  # pacman writes questions to stderr unless noconfirm specified
  let stream = if noconfirm: stdout else: stderr
  stream.write(^Color.blue, ":: ", ^Color.bold, s, ^Color.normal, " ")
  stream.flushFile()
  if noconfirm:
    stream.write("\n")
    stream.flushFile()
    default
  else:
    try:
      discardStream()
      stdin.readLine()
    except EOFError:
      cancel

proc printColonUserChoice*(color: bool, s: string, answers: openArray[char],
  positive: char, negative: char, noconfirm: bool, default: char): char =
  let answersStr = answers
    .map(c => (if c == positive: c.toUpperAscii else: c))
    .foldl(a & "/" & $b, "")

  let input = printColonUserInput(color, s & " [" & answersStr[1 .. ^1] & "]",
    noconfirm, $default, $negative)
  if input.len == 0:
    positive
  elif input.len == 1:
    let c = input[0].toLowerAscii
    if c in answers: c else: negative
  else:
    negative

proc printColonUserChoiceWithHelp*(color: bool, s: string,
  answers: openArray[tuple[c: char, help: Option[string]]], positive: char,
  noconfirm: bool, default: char): char =
  let c = printColonUserChoice(color, s, answers.map(a => a.c) & '?',
    positive, '?', noconfirm, default)

  if c == '?':
    for answer in @answers & ('?', some(tr"view this help")):
      if answer.help.isSome:
        echo("   ", answer.c, " - ", answer.help.unsafeGet)
    printColonUserChoiceWithHelp(color, s, answers, positive, noconfirm, default)
  else:
    c

macro choices*(choices: varargs[untyped]): untyped =
  result = newNimNode(nnkBracket)
  for choice in choices:
    case choice.kind:
      of nnkCharLit:
        result.add(newPar(choice, newCall(ident("none"), ident("string"))))
      of nnkPar:
        if choice.len == 2:
          result.add(newPar(choice[0], newCall(ident("some"), choice[1])))
        else:
          error("error")
      else:
        error("error")

proc printProgressFull*(bar: bool, title: string): ((string, float) -> void, () -> void) =
  let width = getWindowSize().width

  if not bar or width <= 0:
    echo(title, "...")
    (proc (a: string, c: float) {.closure.} = discard, proc {.closure.} = discard)
  else:
    let infoLen = max(width * 6 / 10, 50).int
    let progressLen = width - infoLen
    let startTime = getTime().toUnix

    var lastTime = startTime
    var lastProgress = 0f
    var averageSpeed = -1f

    proc update(prefix: string, progress: float) {.closure.} =
      let progressTrim = max(min(1, progress + 0.005), 0)
      let progressStr = $(progressTrim * 100).int & "%"
      let paddedProgressStr = ' '.repeat(5 - progressStr.len) & progressStr

      let indicator = if progressLen > 8: (block:
          let fullLen = progressLen - 8
          let barLen = (fullLen.float * progressTrim).int
          " [" & '#'.repeat(barLen) & '-'.repeat(fullLen - barLen) & "]")
        else:
          ""

      let time = getTime().toUnix
      if progress > lastProgress and time > lastTime:
        let speed = (progress - lastProgress) / (time - lastTime).float
        lastTime = time
        lastProgress = progress
        if averageSpeed < 0:
          averageSpeed = speed
        else:
          const factor = 0.25
          averageSpeed = factor * speed + (1 - factor) * averageSpeed

      let timeLeft = if averageSpeed > 0: (block:
          let secondsLeft = ((1 - progress) / averageSpeed).int
          let seconds = secondsLeft %% 60
          let minutes = secondsLeft /% 60
          let secondsStr = if seconds < 10: "0" & $seconds else: $seconds
          let minutesStr = if minutes < 10: "0" & $minutes else: $minutes
          minutesStr & ":" & secondsStr)
        else:
          "--:--"

      stdout.write(prefix, title,
        ' '.repeat(infoLen - prefix.runeLen - title.runeLen - 1 - timeLeft.len),
        ' ', timeLeft, indicator, paddedProgressStr, "\x1b[0K\r")
      stdout.flushFile()

    proc terminate() {.closure.} =
      echo()

    update(" ", 0)

    (update, terminate)

proc printProgressShare*(bar: bool, title: string): ((int, int) -> void, () -> void) =
  let (updateFull, terminate) = printProgressFull(bar, title)

  proc update(current: int, total: int) {.closure.} =
    let prefix = if total > 0:
        "(" & ' '.repeat(($total).len - ($current).len) & $current & "/" &
          $total & ") "
      else:
        " "

    updateFull(prefix, current / total)

  (update, terminate)

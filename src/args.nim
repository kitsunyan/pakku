import
  options, os, posix, sequtils, sets, strutils, sugar,
  utils

type
  ArgumentType* {.pure.} = enum
    short, long, target

  Argument* = tuple[
    key: string,
    value: Option[string],
    atype: ArgumentType
  ]

  OptionPair* = tuple[
    short: Option[string],
    long: string
  ]

  OptionKey* = tuple[
    key: string,
    long: bool
  ]

iterator readLines(): string =
  try:
    while true:
      yield readLine(stdin)
  except:
    discard

iterator splitSingle(valueFull: string, optionsWithParameter: HashSet[OptionKey],
  next: Option[string]): tuple[key: string, value: Option[string], consumedNext: bool] =
  var i = 0
  while i < valueFull.len:
    let key = $valueFull[i]
    if key == "-":
      raise commandError(trc("%s: invalid option -- '%c'\n") %
        [getAppFilename(), key], showError = false)
    elif (key, false) in optionsWithParameter:
      if i == valueFull.high:
        if next.isNone:
          raise commandError(trc("%s: option requires an argument -- '%c'\n") %
            [getAppFilename(), key], showError = false)
        else:
          yield (key, next, true)
      else:
        yield (key, some(valueFull[i + 1 .. ^1]), false)
      i = valueFull.len
    else:
      yield (key, none(string), false)
      i += 1

proc splitArgs*(params: seq[string],
  optionsWithParameter: HashSet[OptionKey], longOptions: seq[string]): seq[Argument] =
  proc handleCurrentNext(current: string, next: Option[string],
    stdinConsumed: bool, endOfOpts: bool): (seq[Argument], Option[string], bool, bool) =
    if current == "-":
      if stdinConsumed or isatty(0) == 1:
        raise commandError(trp("argument '-' specified without input on stdin\n").strip)
      else:
        let args = toSeq(readLines())
          .filter(s => s.len > 0)
          .map(s => (s, none(string), ArgumentType.target))

        return (args, next, true, endOfOpts)
    elif endOfOpts:
      return (@[(current, none(string), ArgumentType.target)], next, stdinConsumed, true)
    elif current == "--":
      return (@[], next, stdinConsumed, true)
    elif current[0 .. 1] == "--":
      let valueFull = current[2 .. ^1]
      let index = valueFull.find("=")
      let keyCandidate = if index >= 0: valueFull[0 .. index - 1] else: valueFull
      let valueOption = if index >= 0: some(valueFull[index + 1 .. ^1]) else: none(string)

      let abbreviatedKeys = longOptions.filter(o => o.find(keyCandidate) == 0)
      let keyOption = if abbreviatedKeys.len == 1:
          some(abbreviatedKeys[0])
        elif abbreviatedKeys.contains(keyCandidate):
          some(keyCandidate)
        else:
          none(string)
      let key = keyOption.get(keyCandidate)

      if keyOption.isNone and abbreviatedKeys.len >= 2:
        raise commandError(trc"%s: option '%s%s' is ambiguous; possibilities:" %
          [getAppFilename(), "--", keyCandidate] &
          abbreviatedKeys.map(s => "'--" & s & "'").foldl(a & " " & b, ""), showError = false)
      elif (key, true) in optionsWithParameter:
        if valueOption.isSome:
          return (@[(key, valueOption, ArgumentType.long)], next, stdinConsumed, false)
        elif next.isSome:
          return (@[(key, next, ArgumentType.long)], none(string), stdinConsumed, false)
        else:
          raise commandError(trc("%s: option '%s%s' requires an argument\n") %
            [getAppFilename(), "--", key], showError = false)
      elif valueOption.isSome:
        raise commandError(trc("%s: option '%s%s' doesn't allow an argument\n") %
          [getAppFilename(), "--", key], showError = false)
      else:
        return (@[(key, none(string), ArgumentType.long)], next, stdinConsumed, false)
    elif current[0] == '-' and current.len >= 2:
      let argsResult = toSeq(splitSingle(current[1 .. ^1], optionsWithParameter, next))
      let consumedNext = argsResult.map(a => a.consumedNext).foldl(a or b)
      let newNext = next.filter(n => not consumedNext)

      return (lc[(x.key, x.value, ArgumentType.short) | (x <- argsResult), Argument],
        newNext, stdinConsumed, false)
    else:
      return (@[(current, none(string), ArgumentType.target)], next, stdinConsumed, false)

  type
    ParseArgument = tuple[arg: Option[Argument],current: Option[string]]
    ParseCycle = tuple[args: seq[ParseArgument], stdinConsumed: bool, endOfOpts: bool]

  proc buildArgs(input: ParseCycle, next: Option[string]): ParseCycle =
    if input.args.len == 0:
      (@[(none(Argument), next)], input.stdinConsumed, input.endOfOpts)
    else:
      let last = input.args[^1]
      if last.current.isSome:
        let handleResult: tuple[args: seq[Argument], next: Option[string],
          stdinConsumed: bool, endOfOpts: bool] = handleCurrentNext(last.current.unsafeGet,
            next, input.stdinConsumed, input.endOfOpts)

        let append = handleResult.args.map(a => (some(a), none(string)))
        (input.args[0 .. ^2] & append & (none(Argument), handleResult.next),
          handleResult.stdinConsumed, handleResult.endOfOpts)
      elif next.isSome:
        (input.args & (none(Argument), next), input.stdinConsumed, input.endOfOpts)
      else:
        input

  let cycle: ParseCycle = (params.map(some) & none(string))
    .foldl(buildArgs(a, b), (newSeq[ParseArgument](), false, false))

  if cycle.stdinConsumed:
    discard close(0)
    discard open("/dev/tty", O_RDONLY)

  lc[x | (y <- cycle.args, x <- y.arg), Argument]

proc isShort*(arg: Argument): bool = arg.atype == ArgumentType.short
proc isLong*(arg: Argument): bool = arg.atype == ArgumentType.long
proc isTarget*(arg: Argument): bool = arg.atype == ArgumentType.target

proc collectArg*(arg: Argument): seq[string] =
  if arg.isShort:
    let key = "-" & arg.key
    arg.value.map(v => @[key, v]).get(@[key])
  elif arg.isLong:
    let key = "--" & arg.key
    arg.value.map(v => @[key, v]).get(@[key])
  elif arg.isTarget:
    @[arg.key]
  else:
    @[]

proc len*(op: OptionPair): int =
  if op.short.isSome: 2 else: 1

iterator items*(op: OptionPair): OptionKey =
  if op.short.isSome:
    yield (op.short.unsafeGet, false)
  yield (op.long, true)

proc filter*(args: seq[Argument], removeMatches: bool, keepTargets: bool,
  pairs: varargs[OptionPair]): seq[Argument] =
  let pairsSeq = @pairs
  let argsSet = lc[x | (y <- pairsSeq, x <- y), OptionKey].toSet

  args.filter(arg => (arg.isShort and (removeMatches xor (arg.key, false) in argsSet)) or
    (arg.isLong and (removeMatches xor (arg.key, true) in argsSet)) or
    (arg.isTarget and keepTargets))

template count*(args: seq[Argument], pairs: varargs[OptionPair]): int =
  args.filter(false, false, pairs).len

template check*(args: seq[Argument], pairs: varargs[OptionPair]): bool =
  args.count(pairs) > 0

template whitelisted*(args: seq[Argument], pairs: varargs[OptionPair]): bool =
  args.filter(true, false, pairs).len == 0

proc matchOption*(arg: Argument, pair: OptionPair): bool =
  (arg.isShort and pair.short.map(o => o == arg.key).get(false)) or
    (arg.isLong and arg.key == pair.long)

proc targets*(args: seq[Argument]): seq[string] =
  args.filter(isTarget).map(a => a.key)

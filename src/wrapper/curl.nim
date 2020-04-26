import
  strutils,
  "../utils"

type
  CurlHandle* = object

  CurlInstance* = object
    handle: ptr CurlHandle
    data: seq[char]

  CurlOption {.pure, size: sizeof(cint).} = enum
    followLocation = 52,
    noSignal = 99,
    timeoutMs = 155,
    connectTimeoutMs = 156,
    writeData = 10001,
    url = 10002,
    writeFunction = 20011

  CurlError* = object of CatchableError

{.passL: "-lcurl".}

proc initCurlGlobal*(flags: clong): cint
  {.cdecl, importc: "curl_global_init".}

proc cleanupCurlGlobal*: void
  {.cdecl, importc: "curl_global_cleanup".}

proc newCurlHandle*: ptr CurlHandle
  {.cdecl, importc: "curl_easy_init".}

proc cleanup*(handle: ptr CurlHandle)
  {.cdecl, importc: "curl_easy_cleanup".}

proc errorCurl*(error: cint): cstring
  {.cdecl, importc: "curl_easy_strerror".}

proc setOption*(handle: ptr CurlHandle, option: CurlOption): cint
  {.cdecl, importc: "curl_easy_setopt", varargs.}

proc perform*(handle: ptr CurlHandle): cint
  {.cdecl, importc: "curl_easy_perform".}

proc escape*(handle: ptr CurlHandle, input: cstring, length: cint): cstring
  {.cdecl, importc: "curl_easy_escape".}

proc freeCurl*(data: pointer)
  {.cdecl, importc: "curl_free".}

proc escape*(instance: ref CurlInstance, s: string): string =
  let esc = instance.handle.escape(s, 0)
  if esc != nil:
    let nesc = $esc
    freeCurl(esc)
    nesc
  else:
    ""

proc curlWriteMemory(mem: array[csize_t.high, char], size: csize_t, nmemb: csize_t,
  userdata: ref CurlInstance): csize_t {.cdecl.} =
  let total = size * nmemb
  if total > 0:
    userData.data &= mem[0 .. total - 1]
  total

var refCount = 0

template withCurlGlobal*(body: untyped): untyped =
  block:
    if refCount == 0:
      if initCurlGlobal(0) != 0:
        raise commandError(tr"failed to initialize curl library")
    refCount += 1
    try:
      body
    finally:
      refCount -= 1
      if refCount == 0:
        cleanupCurlGlobal()

template withCurl*(instance: untyped, body: untyped): untyped =
  block:
    let handle = newCurlHandle()
    if handle == nil:
      raise commandError(tr"failed to initialize curl handle")

    var instance: ref CurlInstance
    new(instance)
    instance.handle = handle
    instance.data = newSeq[char]()

    proc raiseError(code: cint) =
      if code != 0:
        let msg = code.errorCurl
        if msg != nil:
          raise newException(CurlError, tr"failed to perform request" & (": $#" % [$msg]))
        else:
          raise newException(CurlError, tr"failed to perform request")

    proc performInternal(url: string, useTimeout: bool): seq[char] =
      let timeoutMs = if useTimeout: 15000 else: 0
      raiseError(handle.setOption(CurlOption.followLocation, (clong) 1))
      raiseError(handle.setOption(CurlOption.noSignal, (clong) 1))
      raiseError(handle.setOption(CurlOption.timeoutMs, (clong) timeoutMs))
      raiseError(handle.setOption(CurlOption.connectTimeoutMs, (clong) timeoutMs))
      raiseError(handle.setOption(CurlOption.url, url))
      raiseError(handle.setOption(CurlOption.writeFunction, cast[pointer](curlWriteMemory)))
      raiseError(handle.setOption(CurlOption.writeData, instance))
      raiseError(handle.perform())
      instance.data

    proc performString(url: string, useTimeout: bool): string =
      let data = performInternal(url, useTimeout)
      var str = newStringOfCap(data.len)
      for c in data:
        str.add(c)
      str

    try:
      body
    finally:
      handle.cleanup()

import future, os, osproc, re, strutils

{.passL: "-lalpm".}

proc vercmp(a: cstring, b: cstring): cint
  {.cdecl, importc: "alpm_pkg_vercmp".}

proc getSourceVersion(relativePath: string): seq[string] =
  let lines = execProcess("/bin/bash", ["-c",
    """source "$1/PKGBUILD" && echo "$epoch" && echo "$pkgver" && echo "$pkgrel"""",
    "bash", relativePath], options = {}).split("\n")
  if lines.len == 4:
    lines[0 .. 2]
  else:
    @[]

proc getSrcInfoVersion(relativePath: string): seq[string] =
  var file: File
  var epoch = ""
  var pkgver = ""
  var pkgrel = ""

  if file.open(relativePath & "/.SRCINFO"):
    try:
      var matches: array[2, string]
      while true:
        let line = file.readLine()
        if line.match(re"[\t\ ]*(\w+)\ =\ (.*)", matches):
          case matches[0]:
            of "epoch":
              epoch = matches[1]
            of "pkgver":
              pkgver = matches[1]
            of "pkgrel":
              pkgrel = matches[1]
            else:
              discard
    except:
      discard
    finally:
      file.close()

  @[epoch, pkgver, pkgrel]

let compareMethod = commandLineParams()[0]
let relativePath = commandLineParams()[1]
let compareVersion = commandLineParams()[2]

let (currentVersion, supported) = if compareMethod == "source":
    (getSourceVersion(relativePath), true)
  elif compareMethod == "srcinfo":
    (getSrcInfoVersion(relativePath), true)
  else:
    (@[], false)

if not supported:
  programResult = 255
elif currentVersion.len != 3:
  programResult = 125
else:
  let epoch = currentVersion[0].strip
  let pkgver = currentVersion[1].strip
  let pkgrel = currentVersion[2].strip

  if pkgver.len == 0 or pkgrel.len == 0:
    programResult = 125
  else:
    let version = if epoch.len > 0:
        epoch & ":" & pkgver & "-" & pkgrel
      else:
        pkgver & "-" & pkgrel

    echo(version)
    if vercmp(compareVersion, version) > 0:
      programResult = 0
    else:
      programResult = 1

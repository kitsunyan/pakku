import future, osproc, strutils

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

proc handleBisect*(params: seq[string]): int =
  let compareMethod = params[0]
  let relativePath = params[1]
  let compareVersion = params[2]

  let (currentVersion, supported) = if compareMethod == "source":
      (getSourceVersion(relativePath), true)
    else:
      (@[], false)

  if not supported:
    255
  elif currentVersion.len != 3:
    125
  else:
    let epoch = currentVersion[0].strip
    let pkgver = currentVersion[1].strip
    let pkgrel = currentVersion[2].strip

    if pkgver.len == 0 or pkgrel.len == 0:
      125
    else:
      let version = if epoch.len > 0:
          epoch & ":" & pkgver & "-" & pkgrel
        else:
          pkgver & "-" & pkgrel

      # this output is required by main program
      echo(version)
      if vercmp(compareVersion, version) > 0: 0 else: 1

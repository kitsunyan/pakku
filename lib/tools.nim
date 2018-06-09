import os, ospaths
import bisect, install

let fileName = paramStr(0).splitFile().name
let appName = getAppFilename().splitFile().name

let paramsFull = commandLineParams()
let (tool, params) = if fileName == appName:
    (paramsFull[0], paramsFull[1 .. ^1])
  else:
    (fileName, paramsFull)

programResult = case tool:
  of "bisect":
    handleBisect(params)
  of "install":
    handleInstall(params)
  else:
    1
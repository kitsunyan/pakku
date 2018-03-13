import
  future, json, options, re, sequtils, sets, strutils, tables,
  package, utils,
  "wrapper/curl"

type
  AurComment* = tuple[
    author: string,
    date: string,
    text: string
  ]

const
  aurUrl* = "https://aur.archlinux.org/"

proc parseRpcPackageInfo(obj: JsonNode): Option[RpcPackageInfo] =
  template optInt64(i: int64): Option[int64] =
    if i > 0: some(i) else: none(int64)

  let base = obj["PackageBase"].getStr
  let name = obj["Name"].getStr
  let version = obj["Version"].getStr
  let descriptionEmpty = obj["Description"].getStr
  let description = if descriptionEmpty.len > 0: some(descriptionEmpty) else: none(string)
  let maintainerEmpty = obj["Maintainer"].getStr
  let maintainer = if maintainerEmpty.len > 0: some(maintainerEmpty) else: none(string)
  let firstSubmitted = obj["FirstSubmitted"].getBiggestInt(0).optInt64
  let lastModified = obj["LastModified"].getBiggestInt(0).optInt64
  let votes = (int) obj["NumVotes"].getBiggestInt(0)
  let popularity = obj["Popularity"].getFloat(0)

  if base.len > 0 and name.len > 0:
    some(RpcPackageInfo(repo: "aur", base: base, name: name, version: version,
      description: description, maintainer: maintainer,
      firstSubmitted: firstSubmitted, lastModified: lastModified,
      votes: votes, popularity: popularity))
  else:
    none(RpcPackageInfo)

template withAur*(body: untyped): untyped =
  withCurlGlobal():
    body

proc obtainPkgBaseSrcInfo(base: string): (string, Option[string]) =
  try:
    withAur():
      withCurl(instance):
        let url = aurUrl & "cgit/aur.git/plain/.SRCINFO?h=" &
          instance.escape(base)
        (performString(url), none(string))
  except CurlError:
    ("", some(getCurrentException().msg))

proc getRpcPackageInfo*(pkgs: seq[string]): (seq[RpcPackageInfo], Option[string]) =
  if pkgs.len == 0:
    (@[], none(string))
  else:
    withAur():
      try:
        withCurl(instance):
          let url = aurUrl & "rpc/?v=5&type=info&arg[]=" & @pkgs
            .deduplicate
            .map(u => instance.escape(u))
            .foldl(a & "&arg[]=" & b)

          let response = performString(url)
          let results = parseJson(response)["results"]
          let table = lc[(x.name, x) | (y <- results, x <- parseRpcPackageInfo(y)),
            (string, RpcPackageInfo)].toTable
          (lc[x | (p <- pkgs, x <- table.opt(p)), RpcPackageInfo], none(string))
      except CurlError:
        (@[], some(getCurrentException().msg))
      except JsonParsingError:
        (@[], some(tr"failed to parse server response"))

proc getAurPackageInfo*(pkgs: seq[string], rpcInfosOption: Option[seq[RpcPackageInfo]],
  arch: string, progressCallback: (int, int) -> void): (seq[PackageInfo], seq[string]) =
  if pkgs.len == 0:
    (@[], @[])
  else:
    withAur():
      progressCallback(0, pkgs.len)

      let (rpcInfos, error) = if rpcInfosOption.isSome:
          (rpcInfosOption.unsafeGet, none(string))
        else:
          getRpcPackageInfo(pkgs)

      if error.isSome:
        (@[], @[error.unsafeGet])
      else:
        type
          ParseResult = tuple[
            infos: seq[PackageInfo],
            error: Option[string]
          ]

        let deduplicated = lc[x.base | (x <- rpcInfos), string].deduplicate
        progressCallback(0, deduplicated.len)

        proc obtainAndParse(base: string, index: int): ParseResult =
          let (srcInfo, operror) = obtainPkgBaseSrcInfo(base)
          progressCallback(index + 1, deduplicated.len)

          if operror.isSome:
            (@[], operror)
          else:
            let pkgInfos = parseSrcInfo("aur", srcInfo, arch,
              aurUrl & base & ".git", none(string), none(string), none(string), rpcInfos)
            (pkgInfos, none(string))

        let parsed = deduplicated.foldl(a & obtainAndParse(b, a.len), newSeq[ParseResult]())
        let infos = lc[x | (y <- parsed, x <- y.infos), PackageInfo]
        let errors = lc[x | (y <- parsed, x <- y.error), string]

        let table = infos.map(i => (i.name, i)).toTable
        (lc[x | (p <- pkgs, x <- table.opt(p)), PackageInfo], errors)

proc findAurPackages*(query: seq[string]): (seq[RpcPackageInfo], Option[string]) =
  if query.len == 0 or query[0].len <= 2:
    (@[], none(string))
  else:
    withAur():
      try:
        withCurl(instance):
          let url = aurUrl & "rpc/?v=5&type=search&by=name&arg=" &
            instance.escape(query[0])

          let response = performString(url)
          let results = parseJson(response)["results"]
          let rpcInfos = lc[x | (y <- results, x <- parseRpcPackageInfo(y)), RpcPackageInfo]

          let filteredRpcInfos = if query.len > 1: (block:
              let queryLow = query[1 .. ^1].map(q => q.toLowerAscii)
              rpcInfos.filter(i => queryLow.map(q => i.name.toLowerAscii.contains(q) or
                i.description.map(d => d.toLowerAscii.contains(q)).get(false)).foldl(a and b)))
            else:
              rpcInfos

          (filteredRpcInfos, none(string))
      except CurlError:
        (@[], some(getCurrentException().msg))

proc downloadAurComments*(base: string): (seq[AurComment], Option[string]) =
  let (content, error) = withAur():
    try:
      withCurl(instance):
        let url = aurUrl & "pkgbase/" & base & "/?comments=all"
        (performString(url), none(string))
    except CurlError:
      ("", some(getCurrentException().msg))

  if error.isSome:
    (@[], error)
  else:
    let commentRe = re("<h4\\ id=\"comment-\\d+\">\\n\\s+(.*)?\\ commented\\ on\\ " &
      "(.*)\\n(?:.*\\n)*?\\s+</h4>\\n\\t\\t<div\\ id=\"comment-\\d+-content\"\\ " &
      "class=\"article-content\">((?:\\n.*)*?)\\n\\t\\t</div>")

    proc transformComment(comment: string): string =
      comment
        # line breaks can leave a space
        .replace("\n", " ")
        # force line break
        .replace("<br />", "\n")
        # paragraphs look like 2 line breaks
        .replace("<p>", "\n\n")
        .replace("</p>", "\n\n")
        # remove tags
        .replace(re"<.*?>", "")
        # multiple spaces become 1 spage
        .replace(re"\ {2,}", " ")
        # strip lines
        .strip.split("\n").map(s => s.strip).foldl(a & "\n" & b).strip
        # don't allow more than 2 line breaks
        .replace(re"\n{2,}", "\n\n")
        # replace mnemonics
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
        .replace("&amp;", "&")

    proc findAllMatches(start: int, found: seq[AurComment]): seq[AurComment] =
      var matches: array[3, string]
      let index = content.find(commentRe, matches, start)
      if index >= 0:
        findAllMatches(index + 1, found & (matches[0].strip, matches[1].strip,
          transformComment(matches[2])))
      else:
        found

    (findAllMatches(0, @[]), none(string))

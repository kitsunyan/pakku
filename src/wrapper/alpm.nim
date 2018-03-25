import
  strutils,
  "../utils"

type
  AlpmHandle* = object
  AlpmDatabase* = object
  AlpmPackage* = object

  AlpmList*[T] = object
    data*: T
    prev*: ptr AlpmList[T]
    next*: ptr AlpmList[T]

  AlpmDepMod* {.pure, size: sizeof(cint).} = enum
    no = 1,
    eq = 2,
    ge = 3,
    le = 4,
    gt = 5,
    lt = 6

  AlpmReason* {.pure, size: sizeof(cint).} = enum
    explicit = 0,
    depend = 1

  AlpmDependency* = object
    name*: cstring
    version*: cstring
    desc*: cstring
    nameHash: culong
    depMod*: AlpmDepMod

  AlpmGroup* = object
    name*: cstring
    packages*: ptr AlpmList[ptr AlpmPackage]

{.passL: "-lalpm".}

proc newAlpmHandle*(root: cstring, dbpath: cstring, err: var cint): ptr AlpmHandle
  {.cdecl, importc: "alpm_initialize".}

proc release*(handle: ptr AlpmHandle): cint
  {.cdecl, importc: "alpm_release".}

proc setArch*(handle: ptr AlpmHandle, arch: cstring): cint
  {.cdecl, importc: "alpm_option_set_arch".}

proc vercmp*(a: cstring, b: cstring): cint
  {.cdecl, importc: "alpm_pkg_vercmp".}

proc errno*(handle: ptr AlpmHandle): cint
  {.cdecl, importc: "alpm_errno".}

proc errorAlpm*(errno: cint): cstring
  {.cdecl, importc: "alpm_strerror".}

proc register*(handle: ptr AlpmHandle, treeName: cstring, level: cint): ptr AlpmDatabase
  {.cdecl, importc: "alpm_register_syncdb".}

proc local*(handle: ptr AlpmHandle): ptr AlpmDatabase
  {.cdecl, importc: "alpm_get_localdb".}

proc packages*(db: ptr AlpmDatabase): ptr AlpmList[ptr AlpmPackage]
  {.cdecl, importc: "alpm_db_get_pkgcache".}

proc groups*(db: ptr AlpmDatabase): ptr AlpmList[ptr AlpmGroup]
  {.cdecl, importc: "alpm_db_get_groupcache".}

proc name*(db: ptr AlpmDatabase): cstring
  {.cdecl, importc: "alpm_db_get_name".}

proc `[]`*(db: ptr AlpmDatabase, name: cstring): ptr AlpmPackage
  {.cdecl, importc: "alpm_db_get_pkg".}

proc base*(pkg: ptr AlpmPackage): cstring
  {.cdecl, importc: "alpm_pkg_get_base".}

proc name*(pkg: ptr AlpmPackage): cstring
  {.cdecl, importc: "alpm_pkg_get_name".}

proc version*(pkg: ptr AlpmPackage): cstring
  {.cdecl, importc: "alpm_pkg_get_version".}

proc arch*(pkg: ptr AlpmPackage): cstring
  {.cdecl, importc: "alpm_pkg_get_arch".}

proc groups*(pkg: ptr AlpmPackage): ptr AlpmList[cstring]
  {.cdecl, importc: "alpm_pkg_get_groups".}

proc reason*(pkg: ptr AlpmPackage): AlpmReason
  {.cdecl, importc: "alpm_pkg_get_reason".}

proc depends*(pkg: ptr AlpmPackage): ptr AlpmList[ptr AlpmDependency]
  {.cdecl, importc: "alpm_pkg_get_depends".}

proc optional*(pkg: ptr AlpmPackage): ptr AlpmList[ptr AlpmDependency]
  {.cdecl, importc: "alpm_pkg_get_optdepends".}

proc provides*(pkg: ptr AlpmPackage): ptr AlpmList[ptr AlpmDependency]
  {.cdecl, importc: "alpm_pkg_get_provides".}

proc replaces*(pkg: ptr AlpmPackage): ptr AlpmList[ptr AlpmDependency]
  {.cdecl, importc: "alpm_pkg_get_replaces".}

proc cfree*(data: pointer)
  {.cdecl, importc: "free", header: "<stdlib.h>".}

proc freeList*[T](list: ptr AlpmList[T])
  {.cdecl, importc: "alpm_list_free".}

proc freeListInner*[T](list: ptr AlpmList[T], fn: proc (data: pointer): void {.cdecl.})
  {.cdecl, importc: "alpm_list_free_inner".}

proc freeListFull*[T](list: ptr AlpmList[T]) =
  list.freeListInner(cfree)
  list.freeList()

template withAlpm*(root: string, db: string, dbs: seq[string], arch: string,
  handle: untyped, alpmDbs: untyped, errors: untyped, body: untyped): untyped =
  block:
    var errno: cint = 0
    let handle = newAlpmHandle(root, db, errno)

    if handle == nil:
      raise commandError(trp("failed to initialize alpm library\n(%s: %s)\n").strip
        .replace("%s", "$#") % [$errno.errorAlpm, db])

    var alpmDbs = newSeq[ptr AlpmDatabase]()
    var errors = newSeq[string]()
    for dbName in dbs:
      let alpmDb = handle.register(dbName, 1 shl 30)
      if alpmDb != nil:
        alpmDbs &= alpmDb
      else:
        errors &= trp("could not register '%s' database (%s)\n").strip
          .replace("%s", "$#") % [dbName, $handle.errno.errorAlpm]

    try:
      discard handle.setArch(arch)
      body
    finally:
      discard handle.release()

iterator items*[T](list: ptr AlpmList[T]): T =
  var listi = list
  while listi != nil:
    yield listi.data
    listi = listi.next

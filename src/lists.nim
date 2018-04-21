import sequtils

type
  List*[T] = ref object of RootObj
    next: List[T]
    data: T

iterator items*[T](list: List[T]): T =
  var l = list
  while l != nil:
    yield l.data
    l = l.next

proc list*[T](items: varargs[T]): List[T] =
  var last: List[T] = nil
  for i in countdown(items.high, items.low):
    new(result)
    result.data = items[i]
    result.next = last
    last = result

proc reversed*[T](list: List[T]): List[T] =
  var last: List[T] = nil
  for item in list:
    new(result)
    result.data = item
    result.next = last
    last = result

proc `^&`*[T](data: T, list: List[T]): List[T] =
  new(result)
  result.next = list
  result.data = data

proc `^&`*[T](data: openarray[T], list: List[T]): List[T] =
  result = list
  for i in countdown(data.high, data.low):
    result = data[i] ^& result

static:
  # lists test
  template testList[T](list: List[T], testSeq: openArray[T]) =
    block:
      let s = toSeq(list.items)
      if s != @testSeq: raise newException(SystemError, "list: " & $s)

  let l1 = list[int]()
  testList(l1, [])
  let l2 = 1 ^& l1
  testList(l2, [1])
  let l3 = list(1, 2, 3)
  testList(l3, [1, 2, 3])
  let l4 = l3.reversed
  testList(l4, [3, 2, 1])
  let l5 = 4 ^& l4
  testList(l5, [4, 3, 2, 1])
  let l6 = 8 ^& [7, 6] ^& 5 ^& l5
  testList(l6, [8, 7, 6, 5, 4, 3, 2, 1])

import macros

# backported list comprehension from earlier nim versions

type ListComprehension = object
var lc*: ListComprehension

template `|`*(lc: ListComprehension, comp: untyped): untyped = lc

macro `[]`*(lc: ListComprehension, comp, typ: untyped): untyped =
  expectLen(comp, 3)
  expectKind(comp, nnkInfix)
  assert($comp[0] == "|")

  result = newCall(newDotExpr(newIdentNode("result"), newIdentNode("add")), comp[1])

  for i in countdown(comp[2].len - 1, 0):
    let x = comp[2][i]
    expectMinLen(x, 1)
    if x[0].kind == nnkIdent and x[0].strVal == "<-":
      expectLen(x, 3)
      result = newNimNode(nnkForStmt).add(x[1], x[2], result)
    else:
      result = newIfStmt((x, result))

  result = newNimNode(nnkCall).add(newNimNode(nnkPar)
    .add(newNimNode(nnkLambda).add(newEmptyNode(), newEmptyNode(), newEmptyNode(),
      newNimNode(nnkFormalParams).add(newNimNode(nnkBracketExpr).add(newIdentNode("seq"), typ)),
      newEmptyNode(), newEmptyNode(), newStmtList(newAssignment(newIdentNode("result"),
        newNimNode(nnkPrefix).add(newIdentNode("@"), newNimNode(nnkBracket))), result))))

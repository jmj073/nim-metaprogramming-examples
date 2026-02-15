import std/macros
import std/sequtils
import sugar

proc formalParams(fn: NimNode): NimNode =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    fn[3]

proc `formalParams=`(fn: NimNode, newFp: NimNode) =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    fn[3] = newFp

iterator flatIdentDefs(params: openArray[NimNode]): NimNode =
    for node in params:
        node.expectKind nnkIdentDefs
        let typ = node[^2]
        let dflt = node[^1]
        for sym in node[0 ..< ^2]:
            yield newIdentDefs(sym, typ, dflt)

func flatFunc(fn: NimNode): NimNode =
    fn.expectKind nnkProcDef
    result = fn.copy()

    var newFp = newNimNode(nnkFormalParams)
    newFp.add(result.formalParams[0])
    result.formalParams = newFp

    for id in flatIdentDefs(fn.formalParams[1 .. ^1]):
        result.formalParams.add(id)

proc curryFunc(fn: NimNode): NimNode =
    fn.expectKind nnkProcDef

    let
        fnSym = fn[0]
        ret = fn.formalParams[0]
        params = fn.formalParams[1 .. ^1]
        paramSyms = params.map((id) => genSym(nskParam, id[0].repr))

    assert(params.len >= 1)

    result = newProc(
        params = [ret, newIdentDefs(paramSyms[^1], params[^1][^2], params[^1][^1])],
        body = newTree(nnkReturnStmt, newCall(fnSym, paramSyms))
    )

    for i in countdown(high(params)-1, low(params)):
        let
            param = params[i]
            retType = newTree(nnkProcTy, result.formalParams, newEmptyNode())

        result = newProc(
            params = [retType, newIdentDefs(paramSyms[i], param[^2], param[^1])],
            body = newTree(nnkReturnStmt, result)
        )

macro curry(fn: typed): untyped =
    let impl = fn.getImpl
    impl.expectKind nnkProcDef
    impl.flatFunc().curryFunc()

proc foo(a, b, c: int): int =
    a + b + c

when isMainModule:
    echo foo(1, 2, 3)
    let curried = curry(foo)
    echo (curried(1)(2)(3))
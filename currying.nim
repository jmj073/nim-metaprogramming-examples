import std/macros

proc formalParams(fn: NimNode): NimNode =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    fn[3]

proc `formalParams=`(fn: NimNode, newFp: NimNode) =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    fn[3] = newFp

macro typedDumpTree(tree: typed): untyped =
    let t = tree.treeRepr
    let impl = tree.getImpl
    let r = impl.treeRepr
    result = quote do:
        echo `t`
        echo "--------------------------------------------------"
        echo `r`

func removeFirstParam(fn: NimNode): NimNode =
    fn.expectKind nnkProcDef
    assert(fn.formalParams.len >= 2)
    result = fn.copy()
    result.formalParams.del(1)

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
    # 4 is Pragma
    fn.expectKind nnkProcDef

    if fn.formalParams.len == 2:
        var res = fn.copy()
        res[0] = newEmptyNode()
        return res

    let curried = fn.removeFirstParam.curryFunc
    # echo curried.repr, "!!!!!"
    var retTy = newNimNode(nnkProcTy)
    retTy.add(curried.formalParams) # add FormalParams
    retTy.add(curried[4]) # add Pragma
    let params = [retTy, fn.formalParams[1]]

    result = newProc(
        params = params,
        body = newAssignment(ident("result"), curried)
    )

macro currying(fn: typed): untyped =
    let impl = fn.getImpl
    impl.expectKind nnkProcDef
    impl.flatFunc().curryFunc()
    # echo result.repr

proc foo(a, b, c: int): int =
    a + b + c

# typedDumpTree(foo)
# dumpTree:
#     proc asdf() =
#         discard


when isMainModule:
    let curried = currying(foo)
    echo (curried(1)(2)(3))
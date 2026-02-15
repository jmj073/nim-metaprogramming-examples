import std/macros

macro typedDumpTree(tree: typed): untyped =
    let t = tree.treeRepr
    let impl = tree.getImpl
    let r = impl.treeRepr
    result = quote do:
        echo `t`
        echo "--------------------------------------------------"
        echo `r`

func removeFirstParam(fn: NimNode): NimNode =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    assert(fn[3].len >= 2)
    result = fn.copy
    result[3].del(1)

iterator flattenIdentDefs(params: openArray[NimNode]): NimNode =
    for node in params:
        node.expectKind nnkIdentDefs
        let typ = node[^2]
        let dflt = node[^1]
        for sym in node[0 ..< ^2]:
            yield newIdentDefs(sym, typ, dflt)

func flattenFunc(fn: NimNode): NimNode =
    # 3 is FormalParams
    fn.expectKind nnkProcDef
    result = fn.copy

    var newFp = newNimNode(nnkFormalParams)
    newFp.add(result[3][0])
    result[3] = newfp

    for id in flattenIdentDefs(fn[3][1 .. ^1]):
        result[3].add(id)

proc currying_func(fn: NimNode): NimNode =
    # 3 is FormalParams
    # 4 is Pragma
    fn.expectKind nnkProcDef

    if fn[3].len == 2:
        var res = fn.copy
        res[0] = newEmptyNode()
        return res

    let curried = fn.removeFirstParam.currying_func
    echo curried.repr, "!!!!!"
    var retTy = newNimNode(nnkProcTy)
    retTy.add(curried[3]) # add FormalParams
    retTy.add(curried[4]) # add Pragma
    let params = [retTy, fn[3][1]]

    result = newProc(
        params = params,
        body = newAssignment(ident("result"), curried)
    )

macro currying(fn: typed): untyped =
    let impl = fn.getImpl
    impl.expectKind nnkProcDef

    var flatten = flattenFunc(impl.copy)

    result = currying_func(flatten)
    result[0] = ident("curried")
    echo result.repr

proc foo(a, b, c: int): int =
    a + b + c

# typedDumpTree(foo)
# dumpTree:
#     proc asdf() =
#         discard


currying(foo)

echo (curried(1)(2)(3))
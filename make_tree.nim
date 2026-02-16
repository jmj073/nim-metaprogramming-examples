import std/macros

proc process(call: NimNode): NimNode =
    call.expectKind nnkCall
    newCall(
        theProc = ident("newTree"),
        args = [call[0], ]
    )

macro makeTree(tree: untyped): untyped =
    echo tree.treeRepr
    tree.expectKind nnkStmtList
    tree.expectLen 1
    result = process(tree[0])

makeTree:
    notsdf
    nnkProcDef:
        asdfsadf
        hi:
            hello
        world:
            bye
            you
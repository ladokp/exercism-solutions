def tree_from_traversals(preorder, inorder):
    if len(preorder) != len(inorder):
        raise ValueError("traversals must have the same length")
    if set(preorder) != set(inorder):
        raise ValueError("traversals must have the same elements")
    if len(preorder) != len(set(preorder)) or len(inorder) != len(set(inorder)):
        raise ValueError("traversals must contain unique items")
    return build_tree(preorder, inorder)


def build_tree(preorder, inorder):
    if not preorder:
        return {}
    root, index = preorder[0], inorder.index(preorder[0])
    return {
        "v": root,
        "l": build_tree(preorder[1 : 1 + index], inorder[:index]),
        "r": build_tree(preorder[index + 1 :], inorder[index + 1 :]),
    }

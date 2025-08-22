"""
This module provides functions to construct a binary tree from preorder and inorder traversals.

Functions:
- tree_from_traversals: Constructs a binary tree using preorder and inorder lists.
- build_tree: Recursively builds the subtree with given traversal details.
"""

from typing import List, Dict, Union


def tree_from_traversals(
    preorder: List[int], inorder: List[int]
) -> Dict[str, Union[int, Dict]]:
    """
    Constructs a binary tree from preorder and inorder traversal lists.

    Parameters:
    preorder (List[int]): The list containing the preorder traversal of the tree.
    inorder (List[int]): The list containing the inorder traversal of the tree.

    Returns:
    Dict[str, Union[int, Dict]]: A dictionary representing the root node of the
    constructed binary tree.

    Raises:
    ValueError: If the input lists do not have the same length, contain
    non-unique items, or do not have the same elements.
    """
    if len(preorder) != len(inorder):
        raise ValueError("traversals must have the same length")
    if set(preorder) != set(inorder):
        raise ValueError("traversals must have the same elements")
    if len(preorder) != len(set(preorder)) or len(inorder) != len(set(inorder)):
        raise ValueError("traversals must contain unique items")
    return build_tree(preorder, inorder, {v: i for i, v in enumerate(inorder)})


def build_tree(
    preorder: list[int],
    inorder: list[int],
    inorder_index_map: dict[int, int],
    indizes: tuple[int, int, int | None] = (0, 0, None),
) -> dict[str, int | dict]:
    """
    Constructs a subtree from preorder and inorder traversal sequences.

    Args:
    preorder (list[int]): The preorder traversal sequence.
    inorder (list[int]): The inorder traversal sequence.
    inorder_index_map (dict[int, int]): A mapping of values to their indices
                                        in the inorder sequence.
    indizes (tuple[int, int, int | None]): Tuple containing the start and end indices for traversals.

    Returns:
    dict[str, int | dict]: A dictionary representing the subtree with root, left, and right nodes.
    """
    pre_start, in_start, in_end = indizes
    if in_end is None:
        in_end = len(inorder)
    if pre_start >= len(preorder) or in_start >= in_end:
        return {}

    root_val = preorder[pre_start]
    index = inorder_index_map[root_val]
    left_size = index - in_start

    return {
        "v": root_val,
        "l": build_tree(
            preorder,
            inorder,
            inorder_index_map,
            (pre_start + 1, in_start, index),
        ),
        "r": build_tree(
            preorder,
            inorder,
            inorder_index_map,
            (
                pre_start + 1 + left_size,
                index + 1,
                in_end,
            ),
        ),
    }

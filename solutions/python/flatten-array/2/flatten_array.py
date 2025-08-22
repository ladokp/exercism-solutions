"""
This module provides a utility function to flatten nested iterables.
"""

from collections.abc import Iterable


def flatten(nested_iterable):
    """
    Flattens a nested iterable (e.g., list or tuple) into a single list.

    Args:
        nested_iterable: A list or tuple that may contain nested lists or tuples.

    Returns:
        A flat list with all the elements from the nested iterable.
    """
    flat_list = []
    for element in nested_iterable:
        if all(
            (
                not isinstance(element, str),
                isinstance(element, Iterable),
            )
        ):
            flat_list += flatten(element)
        elif element is not None:
            flat_list.append(element)
    return flat_list

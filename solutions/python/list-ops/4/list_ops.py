"""
This module provides a collection of functional programming utilities for list
operations. These functions allow you to perform various operations on lists,
including folding (reducing), mapping, filtering, concatenating, and more.
They are designed to work with iterable data structuresand provide
functional-style operations.

Functions: foldl(function, list_, accumulator), append(list1, list2),
concat(lists), filter(function, list_), length(list_), map(function, list_),
foldr(function, list_, accumulator), reverse(list_).

Usage: from functional_utils import foldl, append, concat, filter, length, map,
foldr, reverse.

Example:
>>> foldl(lambda x, y: x + y, [1, 2, 3], 0)
6
"""
from typing import Callable, List, Any, Iterable


def foldl(
    function: Callable[[Any, Any], Any], list_: Iterable, accumulator: Any
) -> Any:
    """
    Perform a left fold (reduce) operation on a list using a binary function.

    Args:
        function (Callable[[Any, Any], Any]): A binary function that takes two
        arguments and combines them.
        list_ (Iterable): The list to perform the fold operation on.
        accumulator (Any): The initial accumulator value.

    Returns:
        Any: The result of applying the fold operation on the list.

    Example:
        >>> foldl(lambda x, y: x + y, [1, 2, 3], 0)
        6
    """
    if not list_:
        return accumulator
    head, *tail = list_
    return foldl(function, tail, function(accumulator, head))


def append(list1: List, list2: List) -> List:
    """
    Concatenate two lists, list2 is appended to list1.

    Args:
        list1 (List): The first list.
        list2 (List): The second list to be appended to the first.

    Returns:
        List: A new list containing the elements of list1 followed by the
        elements of list2.

    Example:
        >>> append([1, 2], [3, 4])
        [1, 2, 3, 4]
    """
    return foldl(lambda accumulator, x: accumulator + [x], list2, list1)


def concat(lists: Iterable[List]) -> List:
    """
    Concatenate multiple lists into a single list.

    Args:
        lists (Iterable[List]): A sequence of lists to concatenate.

    Returns:
        List: A new list containing all the elements from the input lists.

    Example:
        >>> concat([[1, 2], [3, 4], [5, 6]])
        [1, 2, 3, 4, 5, 6]
    """
    return foldl(lambda accumulator, x: accumulator + x, lists, [])


def filter(function: Callable[[Any], bool], list_: Iterable) -> List:
    """
    Filter elements from a list based on a given predicate function.

    Args:
        function (Callable[[Any], bool]): A function that returns a boolean
        value based on an input element.
        list_ (Iterable): The list to filter.

    Returns:
        List: A list containing elements for which the function returns True.

    Example:
        >>> filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5, 6])
        [2, 4, 6]
    """
    return foldl(
        lambda accumulator, x: append(accumulator, [x])
        if function(x)
        else accumulator,
        list_,
        [],
    )


def length(list_: Iterable) -> int:
    """
    Calculate the length (number of elements) of a list.

    Args:
        list_ (Iterable): The list for which to determine the length.

    Returns:
        int: The number of elements in the input list.

    Example:
        >>> length([1, 2, 3, 4, 5])
        5
    """
    return foldl(lambda x, _: x + 1, list_, 0)


def map(function: Callable[[Any], Any], list_: Iterable) -> List:
    """
    Apply a function to each element of a list and return a new list with the
    results.

    Args:
        function (Callable[[Any], Any]): A function to apply to each element of
        the input list.
        list_ (Iterable): The list to map the function over.

    Returns:
        List: A new list containing the results of applying the function to each
        element.

    Example:
        >>> map(lambda x: x * 2, [1, 2, 3, 4, 5])
        [2, 4, 6, 8, 10]
    """
    return foldl(
        lambda accumulator, x: append(accumulator, [function(x)]), list_, []
    )


def foldr(
    function: Callable[[Any, Any], Any], list_: Iterable, accumulator: Any
) -> Any:
    """
    Perform a right fold (reduce) operation on a list using a binary function.

    Args:
        function (Callable[[Any, Any], Any]): A binary function that takes two
        arguments and combines them.
        list_ (Iterable): The list to perform the fold operation on.
        accumulator (Any): The initial accumulator value.

    Returns:
        Any: The result of applying the fold operation on the list, with elements
        processed in reverse order.

    Example:
        >>> foldr(lambda x, y: x - y, [1, 2, 3], 0)
        -6
    """
    return foldl(function, reverse(list_), accumulator)


def reverse(list_: Iterable) -> List:
    """
    Reverse the order of elements in a list.

    Args:
        list_ (Iterable): The list to reverse.

    Returns:
        List: A new list with elements in the reverse order of the input list.

    Example:
        >>> reverse([1, 2, 3, 4, 5])
        [5, 4, 3, 2, 1]
    """
    return foldl(lambda accumulator, x: append([x], accumulator), list_, [])

"""
This module provides a decorated function to generate Pascal's triangle rows.
"""

from math import comb
from typing import Callable, List


def check_row_count(
    function: Callable[[int], List[List[int]]]
) -> Callable[[int], List[List[int]]]:
    """
    Decorator to ensure that the row count is a non-negative integer.

    Args:
        function (Callable[[int], List[List[int]]]): The function to be decorated.

    Returns:
        Callable[[int], List[List[int]]]: The decorated function.
    """

    def inner(row_count: int) -> List[List[int]]:
        if row_count < 0:
            raise ValueError("number of rows is negative")
        if not row_count:
            return []

        return function(row_count)

    return inner


@check_row_count
def rows(row_count: int) -> List[List[int]]:
    """
    Generates the first `row_count` rows of Pascal's triangle.

    Args:
        row_count (int): Number of rows to generate.

    Returns:
        List[List[int]]: A list of lists, where each inner list represents a row in Pascal's triangle.
    """
    return rows(row_count - 1) + [
        [comb(row_count - 1, i) for i in range(row_count)]
    ]

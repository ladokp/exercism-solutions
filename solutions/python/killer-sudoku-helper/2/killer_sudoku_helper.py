"""
This module provides a utility function to find combinations of numbers
that sum up to a target value, excluding specified numbers.
"""

import itertools


def combinations(target, size, exclude):
    """
    Finds all combinations of a specified size of numbers, in the range from 1 to 9,
    that sum up to a given target, while excluding certain numbers.

    Parameters:
    - target (int): The sum that each combination must add up to.
    - size (int): The number of elements in each combination.
    - exclude (list of int): Numbers that should not be included in any combination.

    Returns:
    - List[List[int]]: A list of combinations, where each combination is a list of integers.
    """
    return [
        list(combo)
        for combo in itertools.combinations(
            (
                index
                for index in range(1, min(target, 9) + 1)
                if index not in exclude
            ),
            size,
        )
        if sum(combo) == target
    ]

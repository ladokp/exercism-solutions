"""
This module provides a function to calculate the sum of all numbers below a given limit
that are multiples of any numbers in a specified list of multiples.
"""


def sum_of_multiples(limit, multiples):
    """Calculate the sum of all numbers below a given limit that are multiples of any numbers in a specified list.

    Args:
        limit (int): The upper limit for the range of numbers to check.
        multiples (list of int): A list of numbers to find multiples of.

    Returns:
        int: The sum of all numbers below the limit that are multiples of any numbers in the list.
    """
    return sum(
        number
        for number in range(1, limit)
        if any(j for j in multiples if j != 0 and number % j == 0)
    )

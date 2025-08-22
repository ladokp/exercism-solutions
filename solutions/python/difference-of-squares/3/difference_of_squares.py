"""
This module provides functions to calculate the square of the sum,
sum of squares, and the difference of squares for a given number.
"""


def square_of_sum(number: int) -> float:
    """
    Calculate the square of the sum of the first 'number' natural numbers.

    Args:
        number (int): The upper limit of natural numbers to include.

    Returns:
        float: The square of the sum of the first 'number' natural numbers.
    """
    return (number * (number + 1) / 2) ** 2


def sum_of_squares(number: int) -> float:
    """
    Calculate the sum of the squares of the first 'number' natural numbers.

    Args:
        number (int): The upper limit of natural numbers to include.

    Returns:
        float: The sum of the squares of the first 'number' natural numbers.
    """
    return number * (number + 1) * (2 * number + 1) / 6


def difference_of_squares(number: int) -> float:
    """
    Calculate the difference between the square of the sum and
    the sum of the squares of the first 'number' natural numbers.

    Args:
        number (int): The upper limit of natural numbers to include.

    Returns:
        float: The difference between the square of the sum and
               the sum of the squares of the first 'number' natural numbers.
    """
    return square_of_sum(number) - sum_of_squares(number)

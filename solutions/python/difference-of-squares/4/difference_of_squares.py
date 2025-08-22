"""
This module provides functions to calculate the square of the sum,
sum of squares, and the difference of squares for a given number.
"""


def sum_of_first_n_natural_numbers(number: int) -> int:
    """
    Calculate the sum of the first n natural numbers using the formula n*(n+1)/2.

    Parameters:
        number (int): The number of terms to sum. Must be a non-negative integer.

    Returns:
        int: The sum of all natural numbers from 1 to n.

    Example:
        >>> sum_of_first_n_natural_numbers(5)
        15
        >>> sum_of_first_n_natural_numbers(6)
        21
    """
    return number * (number + 1) // 2


def square_of_sum(number: int) -> float:
    """
    Calculate the square of the sum of the first 'number' natural numbers.

    Args:
        number (int): The upper limit of natural numbers to include.

    Returns:
        float: The square of the sum of the first 'number' natural numbers.
    """
    return sum_of_first_n_natural_numbers(number) ** 2


def sum_of_squares(number: int) -> float:
    """
    Calculate the sum of the squares of the first 'number' natural numbers.

    Args:
        number (int): The upper limit of natural numbers to include.

    Returns:
        float: The sum of the squares of the first 'number' natural numbers.
    """
    return sum_of_first_n_natural_numbers(number) * (2 * number + 1) // 3


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

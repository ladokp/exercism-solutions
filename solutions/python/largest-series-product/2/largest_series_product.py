"""
Module to calculate the largest product of a specified size in a series of digits.
"""

from functools import reduce


def largest_product(series, span_size):
    """
    Calculate the largest product of consecutive digits in a series.

    Parameters:
    series (str): The string of digits to evaluate.
    span_size (int): The number of consecutive digits to consider for the product.

    Returns:
    int: The largest product of the specified number of consecutive digits.

    Raises:
    ValueError: If the size is negative or greater than the length of the series,
                or if the series contains non-digit characters.
    """
    if span_size == 0:
        return 1
    if span_size < 0:
        raise ValueError("span must not be negative")
    if (total_length := len(digit_series := str(series))) < span_size:
        raise ValueError("span must be smaller than string length")
    if not digit_series.isdigit():
        raise ValueError("digits input must only contain digits")

    return max(
        reduce(
            lambda x, y: x * y,
            tuple(int(n) for n in digit_series)[index : index + span_size],
        )
        for index in range(total_length - (span_size - 1))
    )

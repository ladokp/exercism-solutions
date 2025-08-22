"""
Module to generate contiguous substrings (slices) of a specified length from a given series.
It includes validation checks to ensure valid input parameters.
"""


def validate_slices(func: callable) -> callable:
    """
    Decorator to validate input parameters for the slices function.

    It checks the following conditions:
    - The series should not be empty.
    - The length should not exceed the length of the series.
    - The length should be a positive integer.

    Args:
        func (callable): The function to be decorated.

    Returns:
        callable: The wrapped function with validation checks.
    """

    def wrapper(series: str, length: int) -> list:
        if series == "":
            raise ValueError("series cannot be empty")
        if length > len(series):
            raise ValueError(
                "slice length cannot be greater than series length"
            )
        if length == 0:
            raise ValueError("slice length cannot be zero")
        if length < 0:
            raise ValueError("slice length cannot be negative")
        return func(series, length)

    return wrapper


@validate_slices
def slices(series: str, length: int) -> list:
    """
    Generate all possible contiguous substrings of a given length from a series.

    Args:
        series (str): The string from which to generate slices.
        length (int): The length of each slice.

    Returns:
        list: A list of substrings of the specified length from the series.

    Example:
    >>> slices("12345", 2)
    ['12', '23', '34', '45']
    """
    series_length = len(series)
    return [
        series[index : index + length]
        for index in range(series_length)
        if index + length <= series_length
    ]

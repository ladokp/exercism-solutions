"""
This module provides a function to convert numbers into strings based on specific conditions.
"""


def convert(number: int) -> str:
    """
    Convert a number into a string according to divisibility rules.

    For numbers divisible by:
    - 3, include "Pling"
    - 5, include "Plang"
    - 7, include "Plong"

    If the number is not divisible by any of these, return the number as a string.

    Parameters:
    number (int): The number to be converted.

    Returns:
    str: The converted string or the original number as a string.
    """
    return "".join(
        f"Pl{character}ng"
        for condition, character in ((3, "i"), (5, "a"), (7, "o"))
        if not number % condition
    ) or str(number)

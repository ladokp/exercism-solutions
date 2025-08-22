"""
This module provides functions for converting numbers to English words and
performing simple operations.
"""

ONES_TUPLE = (
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "nineteen",
)


def ones(index):
    """
    Returns the value at the specified index from ONES_TUPLE.

    :param index: An integer representing the index.
    :return: The value at the given index in ONES_TUPLE.
    """
    return ONES_TUPLE[index]


TENS_TUPLE = (
    "zero",
    "ten",
    "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety",
)


def tens(index):
    """
    Returns the value at the specified index from TENS_TUPLE.

    :param index: An integer representing the index.
    :return: The value at the given index in TENS_TUPLE.
    """
    return TENS_TUPLE[index]


def say(number):
    """Convert a number into its English words representation.

    Args:
        number (int): The number to be converted. Must be between 0 and 1 trillion (exclusive).

    Returns:
        str: The English words representation of the number.

    Raises:
        ValueError: If the input number is outside the range [0, 1e12).
    """
    if not 0 <= number < 1e12:
        raise ValueError("input out of range")
    _, divisor, func, txt, hyphen = tuple(
        filter(
            lambda x: x[0],
            (
                (number < 20, 1, ones, "", ""),
                (number < 1e2, 10, tens, "", "-"),
                (number < 1e3, 100, ones, " hundred", " "),
                (number < 1e6, 1_000, say, " thousand", " "),
                (number < 1e9, 1_000_000, say, " million", " "),
                (number < 1e12, 1_000_000_000, say, " billion", " "),
            ),
        )
    )[0]
    quotient, rest = divmod(number, divisor)
    return {0: lambda f, q, r, t, h: f(q) + t}.get(
        rest, lambda f, q, r, t, h: f(q) + t + h + say(r)
    )(func, quotient, rest, txt, hyphen)

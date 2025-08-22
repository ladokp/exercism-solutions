"""
This module provides a function to validate ISBN-10 numbers.
"""


def is_valid(isbn):
    """
    Validate the given ISBN-10 number.

    Args:
        isbn (str): The ISBN-10 number to validate, which may include hyphens.

    Returns:
        bool: True if the ISBN-10 number is valid, False otherwise.
    """
    isbn_list = list(isbn.replace("-", ""))
    if len(isbn_list) != 10:
        return False
    if isbn_list[-1] == "X":
        isbn_list[-1] = "10"
    return (
        all(c.isdigit() for c in isbn_list)
        and not sum(
            int(digit) * factor
            for digit, factor in zip(isbn_list, range(10, 0, -1))
        )
        % 11
    )

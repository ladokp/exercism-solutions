"""
Module for counting the number of 1s in the binary representation of an integer.
"""


def egg_count(display_value: int) -> int:
    """
    Counts the number of 1s in the binary representation of a given integer.

    Args:
        display_value (int): The integer to be evaluated.

    Returns:
        int: The count of 1s in the binary representation.
    """
    if display_value:
        return (display_value & 1) + egg_count(display_value // 2)
    return 0

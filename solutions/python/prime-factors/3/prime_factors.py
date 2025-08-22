"""
Compute the prime factors of a given natural number.
A prime number is only evenly divisible by itself and 1.
"""


def factors(value: int) -> list[int]:
    """
    Calculate and return the list of prime factors of a given integer.

    Parameters:
    value (int): The integer to factorize.

    Returns:
    list[int]: A list of prime factors of the given integer.
    """
    factor_list = []
    factor = 2

    while value > 1:
        if value % factor:
            factor += 1
        else:
            factor_list.append(factor)
            value /= factor

    return factor_list

"""
This module provides a function to classify numbers as perfect, abundant, or deficient.
"""


def classify(number):
    """A perfect number equals the sum of its positive divisors.

    :param number: int a positive integer
    :return: str the classification of the input integer
    """
    if number < 1:
        raise ValueError(
            "Classification is only possible for positive integers."
        )

    aliquot_sum = sum(
        divisor
        for divisor in range(1, number // 2 + 1)
        if not (number % divisor)
    )

    match (
        aliquot_sum == number,
        aliquot_sum > number,
    ):
        case (True, _):
            return "perfect"
        case (_, True):
            return "abundant"
        case _:
            return "deficient"

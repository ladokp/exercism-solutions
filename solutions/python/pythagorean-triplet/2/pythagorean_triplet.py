"""
This module provides a function to find all integer triplets (a, b, c)
such that a + b + c equals a given number and the triplet forms a right triangle.
"""

import math


def triplets_with_sum(number):
    """
    Find all integer triplets (a, b, c) such that:
    1. a + b + c = number
    2. The triplet forms a right triangle (a^2 + b^2 = c^2).

    Parameters:
    number (int): The sum of the triplet.

    Returns:
    list: A list of lists, where each inner list represents a triplet [a, b, c].
    """
    number_float = float(number)
    results = []
    for candidate in range(
        int(number_float / 2) - 1, int((math.sqrt(2) - 1) * number_float), -1
    ):
        discriminant = math.sqrt(
            candidate**2 - number_float**2 + 2 * number_float * candidate
        )
        if discriminant == int(discriminant):
            results.append(
                [
                    int((number_float - candidate - discriminant) / 2),
                    int((number_float - candidate + discriminant) / 2),
                    candidate,
                ]
            )
    return results

"""
Module to check the types of triangles based on their side lengths.
"""

from typing import Callable, List


def is_valid_triangle(
    function: Callable[[int], bool]
) -> Callable[[List[int]], bool]:
    """
    Decorator to check if the given sides can form a valid triangle.

    Args:
        function (Callable[[int], bool]): A function that checks the type of triangle
        based on the number of unique sides.

    Returns:
        Callable[[List[int]], bool]: A function that first checks for a valid triangle
        before applying the classification function.
    """

    def inner(sides: List[int]) -> bool:
        return sum(sides) > 2 * max(sides) and function(len(set(sides)))

    return inner


@is_valid_triangle
def equilateral(number_of_unique_sides: int) -> bool:
    """
    Check if the triangle is equilateral.

    Args:
        number_of_unique_sides (int): Number of unique sides in the triangle.

    Returns:
        bool: True if the triangle is equilateral, False otherwise.
    """
    return number_of_unique_sides == 1


@is_valid_triangle
def isosceles(number_of_unique_sides: int) -> bool:
    """
    Check if the triangle is isosceles.

    Args:
        number_of_unique_sides (int): Number of unique sides in the triangle.

    Returns:
        bool: True if the triangle is isosceles, False otherwise.
    """
    return number_of_unique_sides < 3


@is_valid_triangle
def scalene(number_of_unique_sides: int) -> bool:
    """
    Check if the triangle is scalene.

    Args:
        number_of_unique_sides (int): Number of unique sides in the triangle.

    Returns:
        bool: True if the triangle is scalene, False otherwise.
    """
    return number_of_unique_sides == 3

"""
This module provides functionality to calculate resistor values based on color codes.
"""

RESISTOR_COLORS = (
    "black",
    "brown",
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "grey",
    "white",
)
"""
Tuple of color codes used for calculating resistor values.
Each color corresponds to a number from 0 to 9.
"""

color_code = RESISTOR_COLORS.index
"""
Function to get the index of a given color in RESISTOR_COLORS tuple.
"""


def value(colors):
    """
    Calculate the numerical value of a resistor given its color bands.

    :param colors: A list of the first two color bands of the resistor.
    :return: The numerical value as an integer.
    """
    first_color, second_color = colors[:2]
    return color_code(first_color) * 10 + color_code(second_color)

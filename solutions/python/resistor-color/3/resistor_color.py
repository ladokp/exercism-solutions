"""Module for resistor color code calculations."""

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
"""Tuple of resistor colors in order of their corresponding numeric value."""


def color_code(color):
    """Return the numeric value of a resistor color."""
    return RESISTOR_COLORS.index(color)


def colors():
    """Return a list of all resistor colors."""
    return list(RESISTOR_COLORS)

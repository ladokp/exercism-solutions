"""
Module for calculating the resistance value of a resistor based on color bands.
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

value = RESISTOR_COLORS.index


def label(colors):
    """
    Calculate the resistance value of a resistor given its color bands.

    Parameters:
    colors (list of str): The list of color bands on the resistor.

    Returns:
    str: The resistance value as a string with appropriate prefix.
    """
    first_color, second_color, third_color = colors[:3]
    resistance_value = (
        10 * value(first_color) + value(second_color)
    ) * 10 ** value(third_color)

    prefixes = (
        (1_000_000_000, "giga"),
        (1_000_000, "mega"),
        (1_000, "kilo"),
    )
    prefix = ""

    for threshold, name in prefixes:
        if resistance_value >= threshold:
            resistance_value /= threshold
            prefix = name
            break

    return f"{int(resistance_value)} {prefix}ohms"

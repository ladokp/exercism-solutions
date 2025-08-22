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


def value(colors):
    first_color, second_color = colors[:2]
    return RESISTOR_COLORS.index(first_color) * 10 + RESISTOR_COLORS.index(second_color)

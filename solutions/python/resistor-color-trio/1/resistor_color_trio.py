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


def value(color):
    return RESISTOR_COLORS.index(color)


def label(colors):
    first_color, second_color, third_color = colors[:3]
    resistance_value = (10 * value(first_color) + value(second_color)) * 10 ** value(
        third_color
    )
    prefix = ""
    if resistance_value >= 1_000_000_000:
        resistance_value /= 1_000_000_000
        prefix = "giga"
    elif resistance_value >= 1_000_000:
        resistance_value /= 1_000_000
        prefix = "mega"
    elif resistance_value >= 1_000:
        resistance_value /= 1_000
        prefix = "kilo"
    return f"{int(resistance_value)} {prefix}ohms"

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

RESISTOR_COLORS_TOLERANCE = {
    "silver": "10%",
    "gold": "5%",
    "red": "2%",
    "brown": "1%",
    "green": "0.5%",
    "blue": "0.25%",
    "violet": "0.1%",
    "grey": "0.05%",
}


def value(color):
    return RESISTOR_COLORS.index(color)


def resistor_label(colors):
    if len(colors) == 4:
        first_color, second_color, third_color, tolerance_color = colors[:4]
        resistance_value = (
            10 * value(first_color) + value(second_color)
        ) * 10 ** value(third_color)
    elif len(colors) == 5:
        (
            first_color,
            second_color,
            third_color,
            fourth_color,
            tolerance_color,
        ) = colors[:5]
        resistance_value = (
            100 * value(first_color)
            + 10 * value(second_color)
            + value(third_color)
        ) * 10 ** value(fourth_color)
    else:
        return "0 ohms"
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
    resistance_value = f"{resistance_value}".replace(".0", "")
    return f"{resistance_value} {prefix}ohms ±{RESISTOR_COLORS_TOLERANCE[tolerance_color]}"

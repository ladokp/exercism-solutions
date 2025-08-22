NUM_TO_TEXT = (
    "no",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
)


def recite(start, take=1):
    """
    Recite the verses of the 'Ten Green Bottles' song starting from a given number.

    Args:
        start (int): The starting number of bottles.
        take (int): The number of verses to recite.

    Returns:
        list: A list of strings representing the verses.
    """
    lines = []
    for num in range(start, start - take, -1):
        if num < start:
            lines.append("")
        lines += couplet(num)
    return lines


def couplet(start):
    """
    Generate a couplet for the 'Ten Green Bottles' song for a given number.

    Args:
        start (int): The current number of bottles.

    Returns:
        tuple: A tuple containing 2 initial lines, middle line, and end line.
    """
    begin = line(start, is_initial=True)
    middle = "And if one green bottle should accidentally fall,"
    end = line(start - 1, is_initial=False)
    return begin, begin, middle, end


def line(number, is_initial=False):
    """
    Generate a line of the song based on the number of bottles.

    Args:
        number (int): The current number of bottles.
        is_initial (bool): Indicates if this is the initial line.

    Returns:
        str: The formatted line of the song.
    """
    text = NUM_TO_TEXT[number]
    bottles = f"bottle{'s' if number != 1 else ''}"
    return (
        f"{text.title()} green {bottles} hanging on the wall,"
        if is_initial
        else f"There'll be {text} green {bottles} hanging on the wall."
    )

"""
This module provides functions to convert grid representations of numbers
into their string equivalents. It processes groups of lines that represent
numbers in a specific format and maps them to their numeric values.
"""


def convert_lines(lines):
    """
    Splits the input list of lines into groups of four lines each.

    Parameters:
    lines (list of str): A list where each element is a line of the grid.

    Returns:
    generator: A generator yielding lists, each containing four lines.
    """
    return (lines[index : index + 4] for index in range(0, len(lines), 4))


def convert_line(line):
    """
    Splits a single line into segments of three characters each.

    Parameters:
    line (str): A line of the grid.

    Returns:
    generator: A generator yielding strings, each containing a segment of three characters.
    """
    return (
        "".join(line[index : index + 3]) for index in range(0, len(line), 3)
    )


def map_tup_number(_tup):
    """
    Maps a tuple representing a number in the grid to its string equivalent.

    Parameters:
    _tup (tuple of str): A tuple containing four strings, each representing a portion
                         of the number's grid representation.

    Returns:
    str: The string equivalent of the number if recognized; otherwise "?".
    """
    return {
        (" _ ", "| |", "|_|", "   "): "0",
        ("   ", "  |", "  |", "   "): "1",
        (" _ ", " _|", "|_ ", "   "): "2",
        (" _ ", " _|", " _|", "   "): "3",
        ("   ", "|_|", "  |", "   "): "4",
        (" _ ", "|_ ", " _|", "   "): "5",
        (" _ ", "|_ ", "|_|", "   "): "6",
        (" _ ", "  |", "  |", "   "): "7",
        (" _ ", "|_|", "|_|", "   "): "8",
        (" _ ", "|_|", " _|", "   "): "9",
    }.get(_tup, "?")


def convert(input_grid):
    """
    Converts an input grid of strings into a comma-separated string of numbers.

    Parameters:
    input_grid (list of str): A list where each element represents a line of the grid.

    Returns:
    str: A comma-separated string of numbers extracted from the grid.

    Raises:
    ValueError: If the number of input lines is not a multiple of four or
                the number of input columns is not a multiple of three.
    """
    if len(input_grid) % 4:
        raise ValueError("Number of input lines is not a multiple of four")
    if len(input_grid[0]) % 3:
        raise ValueError("Number of input columns is not a multiple of three")
    output_grid = []
    for group in convert_lines(input_grid):
        list_ = [convert_line(line) for line in group]
        str_ = "".join(map_tup_number(_tup) for _tup in zip(*list_))
        output_grid.append(str_)

    return ",".join(output_grid)

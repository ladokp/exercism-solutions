"""
This module provides functionality to count the number of rectangles formed by '+' characters
in a given list of string lines, where '+' represents potential rectangle corners.
"""

from itertools import product


def rectangles(lines=None):
    """
    Count the number of rectangles that can be formed using '+' characters as corners in the given lines.

    Args:
        lines (list of str): A list of strings representing a grid, where each '+' character is a potential corner
                             of a rectangle.

    Returns:
        int: The total number of rectangles found in the input lines.
             Returns 0 if the input is None or if no rectangles are found.
    """
    if lines is None:
        return 0
    transposed_lines = ["".join(line) for line in zip(*lines)]
    corner_points = []
    for row_index, row in enumerate(lines):
        for column_index, mark in enumerate(row):
            if mark == "+":
                corner_points.append((row_index, column_index))
    rectangle_count = 0
    for (top_row, left_col), (bottom_row, right_col) in product(
        corner_points, repeat=2
    ):
        if bottom_row <= top_row or right_col <= left_col:
            continue
        horizontal_edges = (
            lines[top_row][left_col : right_col + 1]
            + lines[bottom_row][left_col : right_col + 1]
        )
        vertical_edges = (
            transposed_lines[left_col][top_row : bottom_row + 1]
            + transposed_lines[right_col][top_row : bottom_row + 1]
        )
        if (
            " " not in horizontal_edges
            and "|" not in horizontal_edges
            and " " not in vertical_edges
            and "-" not in vertical_edges
        ):
            rectangle_count += 1
    return rectangle_count

"""
This module provides functionality to analyze a flowerfield and count adjacent flowers.

Constants:
- DIRECTIONS: A tuple of tuples indicating the eight possible directions in
              which to check for adjacent flowers.

Functions:
- count_flowers(flowerfield, prow, pcol): Counts adjacent flowers to a given
                                      position in the flowerfield.
- annotate(flowerfield): Annotates the flowerfield with counts of adjacent flowers,
                       returning an updated flowerfield.
"""

DIRECTIONS = (
    (0, 1),
    (1, 0),
    (1, 1),
    (0, -1),
    (-1, 0),
    (-1, -1),
    (-1, 1),
    (1, -1),
)


def count_flowers(flowerfield, prow, pcol):
    """
    Count the number of flowers adjacent to the specified position in the flowerfield.

    Parameters:
    - flowerfield: List of strings representing the flowerfield.
    - prow: Integer indicating the row position of the cell to check.
    - pcol: Integer indicating the column position of the cell to check.

    Returns:
    - A string representing the number of adjacent flowers, or a space if there are no adjacent flowers.
    """
    count = sum(
        flowerfield[prow + direction[0]][pcol + direction[1]] == "*"
        for direction in DIRECTIONS
        if (
            0 <= prow + direction[0] <= len(flowerfield) - 1
            and 0 <= pcol + direction[1] <= len(flowerfield[0]) - 1
        )
    )
    return " " if not count else str(count)


def annotate(flowerfield):
    """
    Annotate the flowerfield by replacing spaces with the number of adjacent flowers.

    Parameters:
    - flowerfield: List of strings representing the flowerfield.

    Returns:
    - A new list of strings representing the annotated flowerfield with counts of adjacent flowers.
    """
    if not flowerfield:
        return []
    column_count = len(flowerfield[0])
    if any(len(column) != column_count for column in flowerfield):
        raise ValueError("The board is invalid with current input.")

    for index_i, _ in enumerate(flowerfield):
        temp_row = ""
        for index_j in range(column_count):
            if flowerfield[index_i][index_j].isspace():
                temp_row += count_flowers(flowerfield, index_i, index_j)
            elif flowerfield[index_i][index_j] == "*":
                temp_row += "*"
            else:
                raise ValueError("The board is invalid with current input.")
        flowerfield[index_i] = temp_row

    return flowerfield

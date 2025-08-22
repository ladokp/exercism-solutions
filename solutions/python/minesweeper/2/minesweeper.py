"""
This module provides functionality to analyze a minefield and count adjacent mines.

Constants:
- DIRECTIONS: A tuple of tuples indicating the eight possible directions in
              which to check for adjacent mines.

Functions:
- count_mines(minefield, prow, pcol): Counts adjacent mines to a given
                                      position in the minefield.
- annotate(minefield): Annotates the minefield with counts of adjacent mines,
                       returning an updated minefield.
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


def count_mines(minefield, prow, pcol):
    """
    Count the number of mines adjacent to the specified position in the minefield.

    Parameters:
    - minefield: List of strings representing the minefield.
    - prow: Integer indicating the row position of the cell to check.
    - pcol: Integer indicating the column position of the cell to check.

    Returns:
    - A string representing the number of adjacent mines, or a space if there are no adjacent mines.
    """
    count = sum(
        minefield[prow + direction[0]][pcol + direction[1]] == "*"
        for direction in DIRECTIONS
        if (
            0 <= prow + direction[0] <= len(minefield) - 1
            and 0 <= pcol + direction[1] <= len(minefield[0]) - 1
        )
    )
    if count == 0:
        return " "
    return str(count)


def annotate(minefield):
    """
    Annotate the minefield by replacing spaces with the number of adjacent mines.

    Parameters:
    - minefield: List of strings representing the minefield.

    Returns:
    - A new list of strings representing the annotated minefield with counts of adjacent mines.
    """
    if not minefield:
        return []
    column_count = len(minefield[0])
    if any(len(column) != column_count for column in minefield):
        raise ValueError("The board is invalid with current input.")

    for index_i, _ in enumerate(minefield):
        temp_row = ""
        for index_j in range(column_count):
            if minefield[index_i][index_j].isspace():
                temp_row += count_mines(minefield, index_i, index_j)
            elif minefield[index_i][index_j] == "*":
                temp_row += "*"
            else:
                raise ValueError("The board is invalid with current input.")
        minefield[index_i] = temp_row

    return minefield

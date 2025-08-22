"""
Module for identifying saddle points in a matrix.

A saddle point is an element which is the largest in its row and the smallest in its column.
This module provides a function to identify all such points in a given matrix.

"""


def saddle_points(matrix):
    """
    Identify all saddle points in the given matrix. A saddle point is an element which is
    the largest in its row and the smallest in its column.

    Args:
        matrix (list of list of int): A 2D list representing the matrix to be evaluated.

    Raises:
        ValueError: If the input matrix is irregular (rows are not of the same length).

    Returns:
        list of dict: A list of dictionaries where each dictionary represents a saddle point,
                      containing keys 'row' and 'column' with 1-based indices.
    """
    if any(len(row) != len(matrix[0]) for row in matrix):
        raise ValueError("irregular matrix")

    row_max_positions = (
        (int(row_index), int(col_index))
        for row_index, row in enumerate(matrix)
        for col_index in range(len(row))
        if row[col_index] == max(row)
    )

    return [
        {"row": position[0] + 1, "column": position[1] + 1}
        for position in row_max_positions
        if matrix[position[0]][position[1]]
        == min(
            matrix[row_index][position[1]] for row_index in range(len(matrix))
        )
    ]

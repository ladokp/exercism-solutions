from itertools import cycle


def spiral_matrix(matrix_size: int) -> list[list[int]]:
    """
    Create a spiral matrix of a given size.

    Args:
        matrix_size (int): The size of the matrix (number of rows and columns).

    Returns:
        list[list[int]]: A 2D list representing the spiral matrix.
    """
    spiral_matrix_ = [[None] * matrix_size for _ in range(matrix_size)]
    row_index, col_index = 0, 0
    direction_cycle = cycle(((0, 1), (1, 0), (0, -1), (-1, 0)))
    delta_row, delta_col = next(direction_cycle)
    for index in range(matrix_size**2):
        spiral_matrix_[row_index][col_index] = index + 1
        if (
            not 0 <= row_index + delta_row < matrix_size
            or not 0 <= col_index + delta_col < matrix_size
            or spiral_matrix_[row_index + delta_row][col_index + delta_col]
            is not None
        ):
            delta_row, delta_col = next(direction_cycle)
        row_index += delta_row
        col_index += delta_col
    return spiral_matrix_

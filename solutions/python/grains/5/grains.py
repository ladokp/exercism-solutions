"""
This module calculates the number of grains of wheat on a specific square
of a chessboard and the total number of grains on the entire board,
given that the number of grains doubles on each subsequent square.
"""

MAX_SQUARES = 64


def square(square_number):
    """
    Calculate the number of grains of wheat on a specific square of a chessboard
    when the number of grains doubles on each subsequent square.

    Args:
        square_number (int): The position of the square (must be between 1 and 64).

    Returns:
        int: The number of grains on the given square.

    Raises:
        ValueError: If 'square_' is not between 1 and MAX_SQUARES.
    """
    if not 1 <= square_number <= MAX_SQUARES:
        raise ValueError(f"square must be between 1 and {MAX_SQUARES}")
    return 1 << (square_number - 1)


def total():
    """
    Calculate the total number of grains of wheat on a chessboard
    where the number of grains doubles on each subsequent square.

    Returns:
        int: The total number of grains on the chessboard.
    """
    return (1 << MAX_SQUARES) - 1

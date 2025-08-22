"""
Matrix Module

This module provides a Matrix class that represents a matrix
with functionalities to extract rows and columns as tuples and lists.
"""


class Matrix:
    """
    A class to represent a matrix with capabilities to extract its rows and columns.

    The matrix is initialized using a string representation, where each line corresponds
    to a row and numbers within a line correspond to elements of the row. The class provides
    methods to retrieve these rows and columns in different formats.
    """

    def __init__(self, matrix_string: str) -> None:
        """
        Initialize the Matrix object with a string representation of the matrix.

        :param matrix_string: A multi-line string where each line represents a row
                              of the matrix and each number is an element of the row,
                              separated by spaces.
        """
        self.matrix = tuple(
            [int(number) for number in line.split()]
            for line in matrix_string.splitlines()
        )

    def row(self, index: int) -> tuple:
        """
        Return the specified row of the matrix as a tuple.

        :param index: The 1-based index of the row to retrieve.
        :return: A tuple containing the elements of the specified row.
        """
        return self.matrix[index - 1]

    def column(self, index: int) -> list:
        """
        Return the specified column of the matrix as a list.

        :param index: The 1-based index of the column to retrieve.
        :return: A list containing the elements of the specified column.
        """
        return [row[index - 1] for row in self.matrix]

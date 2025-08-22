class Matrix:
    def __init__(self, matrix_string: str) -> None:
        """
        Initialize the Matrix with a string representation of the matrix.

        :param matrix_string: A string where each line represents a row in the matrix.
        """
        self.matrix = tuple(
            [int(number) for number in line.split()]
            for line in matrix_string.splitlines()
        )

    def row(self, index: int) -> tuple:
        """
        Return the specified row of the matrix.

        :param index: The 1-based index of the row to return.
        :return: A tuple representing the specified row.
        """
        return self.matrix[index - 1]

    def column(self, index: int) -> list:
        """
        Return the specified column of the matrix.

        :param index: The 1-based index of the column to return.
        :return: A list representing the specified column.
        """
        return [row[index - 1] for row in self.matrix]

"""Module for the Queen class which represents a Queen on a chessboard."""


class Queen:
    """Represents a Queen piece on an 8x8 chessboard."""

    def __init__(self, row, column):
        """Initialize a Queen at the given row and column on the board.

        Args:
            row (int): The row position of the Queen (0-7).
            column (int): The column position of the Queen (0-7).

        Raises:
            ValueError: If the row or column is not within the valid range.
        """
        if row < 0:
            raise ValueError("row not positive")
        if row > 7:
            raise ValueError("row not on board")
        if column < 0:
            raise ValueError("column not positive")
        if column > 7:
            raise ValueError("column not on board")
        self.row = row
        self.column = column

    def can_attack(self, other):
        """Determine if this Queen can attack another Queen.

        Args:
            other (Queen): Another Queen object to check against.

        Returns:
            bool: True if this Queen can attack the other Queen, False otherwise.

        Raises:
            ValueError: If both Queens are in the same position.
        """
        if self.row == other.row and self.column == other.column:
            raise ValueError(
                "Invalid queen position: both queens in the same square"
            )
        row_difference = abs(self.row - other.row)
        column_difference = abs(self.column - other.column)
        return (
            row_difference == 0
            or column_difference == 0
            or row_difference == column_difference
        )

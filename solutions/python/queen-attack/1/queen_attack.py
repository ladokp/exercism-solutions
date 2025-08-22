class Queen:
    def __init__(self, row, column):
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
        if self.row == other.row and self.column == other.column:
            raise ValueError(
                "Invalid queen position: both queens in the same square"
            )
        d_row = abs(self.row - other.row)
        d_column = abs(self.column - other.column)
        return d_row == 0 or d_column == 0 or d_row == d_column

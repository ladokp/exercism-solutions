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
    else:
        return str(count)


def annotate(minefield):
    if not minefield:
        return []
    else:
        row_count = len(minefield)
        column_count = len(minefield[0])
        for col in minefield:
            if len(col) != column_count:
                raise ValueError("The board is invalid with current input.")

        for index_i in range(row_count):
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

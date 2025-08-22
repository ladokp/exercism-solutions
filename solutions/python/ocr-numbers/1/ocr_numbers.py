def convert_lines(lines):
    return [lines[index : index + 4] for index in range(0, len(lines), 4)]


def convert_line(line):
    return [
        "".join(line[index : index + 3]) for index in range(0, len(line), 3)
    ]


def map_tup_number(_tup):
    return {
        (" _ ", "| |", "|_|", "   "): "0",
        ("   ", "  |", "  |", "   "): "1",
        (" _ ", " _|", "|_ ", "   "): "2",
        (" _ ", " _|", " _|", "   "): "3",
        ("   ", "|_|", "  |", "   "): "4",
        (" _ ", "|_ ", " _|", "   "): "5",
        (" _ ", "|_ ", "|_|", "   "): "6",
        (" _ ", "  |", "  |", "   "): "7",
        (" _ ", "|_|", "|_|", "   "): "8",
        (" _ ", "|_|", " _|", "   "): "9",
    }.get(_tup, "?")


def convert(input_grid):
    if len(input_grid) % 4 != 0:
        raise ValueError("Number of input lines is not a multiple of four")
    if len(input_grid[0]) % 3 != 0:
        raise ValueError("Number of input columns is not a multiple of three")
    groups_of_lines = convert_lines(input_grid)
    output_grid = []
    for group in groups_of_lines:
        _list = [convert_line(line) for line in group]
        _str = "".join(map_tup_number(_tup) for _tup in zip(*_list))
        output_grid.append(_str)

    return ",".join(output_grid)

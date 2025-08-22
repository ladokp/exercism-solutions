from math import comb


def rows(row_count):
    if row_count < 0:
        raise ValueError("number of rows is negative")
    if not row_count:
        return []
    return rows(row_count - 1) + [
        [comb(row_count - 1, i) for i in range(row_count)]
    ]

from functools import reduce


def largest_product(series, size):
    if size == 0:
        return 1
    if size < 0:
        raise ValueError("span must not be negative")
    elif (series_length := len(series := str(series))) < size:
        raise ValueError("span must be smaller than string length")
    elif not series.isdigit():
        raise ValueError("digits input must only contain digits")

    return max(
        reduce(
            lambda x, y: x * y,
            tuple(int(n) for n in series)[index : index + size],
        )
        for index in range(series_length - (size - 1))
    )

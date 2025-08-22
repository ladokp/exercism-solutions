MAX_SQUARES = 64


def square(square_):
    if not 1 <= square_ <= MAX_SQUARES:
        raise ValueError(f"square must be between 1 and {MAX_SQUARES}")
    return 1 << (square_ - 1)


def total():
    return (1 << MAX_SQUARES) - 1

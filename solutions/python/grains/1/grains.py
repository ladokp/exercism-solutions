def square(number):
    if not 1 <= number <= 64:
        raise ValueError("square must be between 1 and 64")
    return int("1" + "0" * (number - 1), 2)


def total():
    return int("1" * 64, 2)

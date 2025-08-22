def steps(number):
    if number <= 0:
        raise ValueError("Only positive integers are allowed")
    return (
        0
        if number == 1
        else 1
        + (steps(number // 2) if not number & 1 else steps(3 * number + 1))
    )

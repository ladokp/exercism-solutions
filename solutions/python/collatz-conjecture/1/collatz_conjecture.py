def steps(number):
    if number <= 0:
        raise ValueError("Only positive integers are allowed")
    return (
        0
        if number == 1
        else 1 + (steps(number // 2) if number % 2 == 0 else steps(3 * number + 1))
    )

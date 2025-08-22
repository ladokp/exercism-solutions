def steps(number):
    """
    Calculate the number of steps required to reduce the given positive integer
    to 1 according to the rules of the Collatz conjecture.

    The rules are:
    - If the number is even, divide it by 2.
    - If the number is odd, multiply it by 3 and add 1.
    - Repeat the process until the number becomes 1.

    Parameters:
    number (int): A positive integer to be reduced to 1.

    Returns:
    int: The number of steps required to reduce the input number to 1.

    Raises:
    ValueError: If the input number is less than 1 (only positive integers are allowed).
    """
    if number < 1:
        raise ValueError("Only positive integers are allowed")

    if number == 1:
        return 0

    return 1 + (steps(3 * number + 1) if number & 1 else steps(number // 2))

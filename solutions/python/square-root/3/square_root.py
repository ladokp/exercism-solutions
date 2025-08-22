def square_root(number, tolerance=1e-8):
    """Compute the square root of a given non-negative number using Newton's method.

    This function estimates the square root by iteratively improving the guess
    until it converges to within a specified tolerance. The algorithm uses
    an initial guess equal to the input number and refines this guess
    through the formula: better_guess = 0.5 * (guess + number / guess).

    Args:
        number (float): The number to compute the square root of. Must be non-negative.
        tolerance (float): The tolerance level for determining convergence. Default is 1e-8.

    Returns:
        float: The estimated square root of the number.

    Raises:
        ValueError: If the input number is negative, indicating that the square root
        is not defined for negative values.
    """
    if number == 0:
        return 0
    if number < 0:
        raise ValueError("Cannot compute the square root of a negative number.")

    guess = number

    while True:
        better_guess = 0.5 * (guess + number / guess)

        if abs(guess - better_guess) < tolerance:
            return better_guess

        guess = better_guess

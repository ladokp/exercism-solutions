def square_root(number, tolerance=1e-8):
    if number == 0:
        return 0
    if number < 0:
        raise ValueError("Cannot compute the square root of a negative number.")

    # Initial guess can be any positive number, for simplicity, we'll take the number itself
    guess = number

    while True:
        # Calculate a better guess using Newton's method
        better_guess = 0.5 * (guess + number / guess)

        # Check if the difference between the guess and the better guess is within the tolerance
        if abs(guess - better_guess) < tolerance:
            return better_guess

        guess = better_guess

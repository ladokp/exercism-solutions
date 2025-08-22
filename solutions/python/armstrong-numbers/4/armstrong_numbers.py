def is_armstrong_number(number):
    """
    Determine if a given number is an Armstrong number.

    An Armstrong number (also known as a narcissistic number) is a number that is equal to the sum of its own digits raised to the power of the number of digits. For example,
    153 is an Armstrong number because 1^3 + 5^3 + 3^3 = 153.

    Args:
        number (int): The number to check.

    Returns:
        bool: True if the number is an Armstrong number, False otherwise.
    """
    string_number = str(number)
    string_length = len(string_number)
    return sum(int(digit) ** string_length for digit in string_number) == number

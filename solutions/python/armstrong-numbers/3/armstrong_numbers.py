def is_armstrong_number(number):
    string_number = str(number)
    string_length = len(string_number)
    return sum(int(digit) ** string_length for digit in string_number) == number

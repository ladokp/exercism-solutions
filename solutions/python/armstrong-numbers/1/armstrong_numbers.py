def is_armstrong_number(number):
    string_number = str(number)
    return (sum(int(digit) ** len(string_number) for digit in string_number)) == number

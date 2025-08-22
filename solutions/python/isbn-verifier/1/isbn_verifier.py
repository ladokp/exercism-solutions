def is_valid(isbn):
    isbn_list = list(isbn.replace("-", ""))
    if len(isbn_list) != 10:
        return False
    if isbn_list[-1] == "X":
        isbn_list[-1] = "10"
    if not all([c.isdigit() for c in isbn_list]):
        return False
    checksum = sum(
        int(digit) * factor for digit, factor in zip(isbn_list, range(10, 0, -1))
    )
    return checksum % 11 == 0

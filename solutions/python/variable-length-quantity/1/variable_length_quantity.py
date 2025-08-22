EIGHT_BIT_MASK = 0x80
SEVEN_BIT_MASK = 0x7F


def encode_int(n: int) -> list[int]:
    byte_lst = [n & SEVEN_BIT_MASK]
    n >>= 7
    while n > 0:
        byte_lst.append(n & SEVEN_BIT_MASK | EIGHT_BIT_MASK)
        n >>= 7
    return byte_lst[::-1]


def encode(numbers: list[int]) -> list[int]:
    byte_lst = []
    for n in numbers:
        byte_lst.extend(encode_int(n))
    return byte_lst


def decode(bytes_: list[int]) -> list[int]:
    values = []
    number = 0
    for index, byte in enumerate(bytes_):
        number = (number << 7) + (byte & SEVEN_BIT_MASK)
        if not byte & EIGHT_BIT_MASK:
            values.append(number)
            number = 0
        elif index == len(bytes_) - 1:
            raise ValueError("incomplete sequence")
    return values

def rebase(input_base: int, digits: list[int], output_base: int) -> list[int]:
    """Rebase a number represented as a list of digits from input_base to output_base.

    Parameters:
    input_base (int): The base of the input number, must be >= 2.
    digits (list[int]): The digits of the number in input_base.
    output_base (int): The base to convert the number to, must be >= 2.

    Returns:
    list[int]: The digits of the number in output_base.

    Raises:
    ValueError: If input_base or output_base is less than 2.
    ValueError: If any digit is not within the range of the input_base.
    ValueError: If the digits list represents the number zero with all digits being zero.
    """
    if input_base < 2:
        raise ValueError("input base must be >= 2")
    if not all(0 <= d < input_base for d in digits):
        raise ValueError("all digits must satisfy 0 <= d < input base")
    if output_base < 2:
        raise ValueError("output base must be >= 2")
    if not any(d > 0 for d in digits):
        return [0]

    digits.reverse()
    number = sum(d * input_base**i for i, d in enumerate(digits))
    result = []
    while number > 0:
        result.append(number % output_base)
        number //= output_base
    result.reverse()
    return result

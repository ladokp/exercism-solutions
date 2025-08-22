def rebase(input_base, digits, output_base):
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

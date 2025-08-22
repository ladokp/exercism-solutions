def classify(number):
    """A perfect number equals the sum of its positive divisors.

    :param number: int a positive integer
    :return: str the classification of the input integer
    """
    if number <= 0:
        raise ValueError("Classification is only possible for positive integers.")

    aliquot_sum = sum(divisor for divisor in range(1, number) if not (number % divisor))
    if aliquot_sum == number:
        return "perfect"
    if aliquot_sum > number:
        return "abundant"
    return "deficient"

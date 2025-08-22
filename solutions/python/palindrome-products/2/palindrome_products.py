def check_factors(function):
    """Decorator to check if the minimum factor is less than or equal to the maximum factor."""

    def inner(min_factor: int, max_factor: int) -> tuple:
        """Check and raise ValueError if min_factor is greater than max_factor."""
        if min_factor > max_factor:
            raise ValueError("min must be <= max")
        return function(min_factor, max_factor)

    return inner


@check_factors
def largest(min_factor: int, max_factor: int) -> tuple:
    """
    Find the largest palindromic product of two factors within the given range.

    Parameters:
    min_factor (int): The minimum factor for the product calculation.
    max_factor (int): The maximum factor for the product calculation.

    Returns:
    tuple: A tuple containing the largest palindromic product and a list of
           factor pairs that produce this product.
           If no palindromic product is found, returns (None, []).
    """
    result, answer = 0, []
    for number_a in range(max_factor, min_factor - 1, -1):
        if number_a * max_factor < result:
            break
        for number_b in range(max_factor, number_a - 1, -1):
            product = number_a * number_b
            if product < result:
                break
            test_value = str(product)
            if test_value == test_value[::-1]:
                if product > result:
                    answer = []
                    result = product
                answer.append([number_a, number_b])
    if result == 0:
        result = None
    return result, answer


@check_factors
def smallest(min_factor: int, max_factor: int) -> tuple:
    """
    Find the smallest palindromic product of two factors within the given range.

    Parameters:
    min_factor (int): The minimum factor for the product calculation.
    max_factor (int): The maximum factor for the product calculation.

    Returns:
    tuple: A tuple containing the smallest palindromic product and a list of
           factor pairs that produce this product.
           If no palindromic product is found, returns (None, []).
    """
    result, answer, found = float("inf"), [], False
    for number_a in range(min_factor, max_factor + 1):
        if number_a * min_factor > result and found:
            break
        for number_b in range(min_factor, number_a + 1):
            product = number_a * number_b
            if product > result and found:
                break
            test_value = str(product)
            if test_value == test_value[::-1]:
                if product < result:
                    answer = []
                    result = product
                answer.append([number_a, number_b])
                found = True
    if result == float("inf"):
        result = None
    return result, answer

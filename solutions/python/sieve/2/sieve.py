"""
Module for generating prime numbers up to a given limit using the Sieve of Eratosthenes algorithm.
"""


def primes(limit):
    """
    Generates a list of prime numbers up to a specified limit.

    Args:
        limit (int): The upper limit (inclusive) for finding prime numbers.

    Returns:
        list: A sorted list of prime numbers up to the specified limit.
    """
    if limit < 2:
        return []

    is_prime = [True] * (limit + 1)
    number = 2
    while number * number <= limit:
        if is_prime[number]:
            for i in range(number * number, limit + 1, number):
                is_prime[i] = False
        number += 1

    return [p for p in range(2, limit + 1) if is_prime[p]]

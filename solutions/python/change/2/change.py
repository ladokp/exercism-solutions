from itertools import combinations_with_replacement


def find_fewest_coins(coins, target):
    """Find the fewest coins that make up the target amount.

    Args:
        coins (iterable): The denominations of the coins available.
        target (int): The target amount to make with the coins.

    Raises:
        ValueError: If the target is negative or cannot be made with the given coins.

    Returns:
        list: A sorted list of coins that make up the target amount.
    """
    if target < 0:
        raise ValueError("target can't be negative")

    for index in range(1 + target // min(coins)):
        for combo in combinations_with_replacement(coins, index):
            if sum(combo) == target:
                return sorted(combo)
    raise ValueError("can't make target with given coins")

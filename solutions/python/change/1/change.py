from itertools import combinations_with_replacement


def find_fewest_coins(coins, target):
    if target < 0:
        raise ValueError("target can't be negative")

    for index in range(1 + target // min(coins)):
        for combo in combinations_with_replacement(coins, index):
            if sum(combo) == target:
                return sorted(combo)
    raise ValueError("can't make target with given coins")

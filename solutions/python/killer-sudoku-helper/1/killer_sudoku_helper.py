import itertools


def combinations(target, size, exclude):
    return [
        list(combo)
        for combo in itertools.combinations(
            (
                index
                for index in range(1, min(target, 9) + 1)
                if index not in exclude
            ),
            size,
        )
        if sum(combo) == target
    ]

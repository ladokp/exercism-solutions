def maximum_value(maximum_weight, items):
    combinations = [(0, 0)]
    for item in items:
        combinations += [
            (weight + item["weight"], value + item["value"])
            for weight, value in combinations
        ]

    return max(
        value for weight, value in combinations if weight <= maximum_weight
    )

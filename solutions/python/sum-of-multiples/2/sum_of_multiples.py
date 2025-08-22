def sum_of_multiples(limit, factors):
    return sum(
        {
            number
            for factor in factors
            if factor
            for number in range(factor, limit, factor)
        }
    )

def factors(value):
    factor_list = []
    factor = 2

    while value > 1:
        if value % factor == 0:
            factor_list.append(factor)
            value /= factor
        else:
            factor += 1

    return factor_list

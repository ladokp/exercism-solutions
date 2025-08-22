def sum_of_multiples(limit, multiples):
    if 0 in multiples:
        multiples.remove(0)
    sum_of_multiples_to_return = 0
    for number in range(limit):
        for divisor in multiples:
            if not (number % divisor):
                sum_of_multiples_to_return += number
                break
    return sum_of_multiples_to_return

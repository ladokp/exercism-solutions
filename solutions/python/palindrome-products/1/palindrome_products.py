def largest(min_factor, max_factor):
    if min_factor > max_factor:
        raise ValueError("min must be <= max")
    result = 0
    answer = []
    for number_a in range(max_factor, min_factor - 1,-1):
        was_bigger = False
        for number_b in range(max_factor, number_a - 1, -1):
            if number_a * number_b >= result:
                was_bigger = True
                test_value  = str(number_a * number_b)
                if test_value == test_value[::-1]:
                    if number_a * number_b > result:
                        answer = []
                        result = int(test_value)
                    answer.append([number_a, number_b])
        if not was_bigger:
            break
    if result == 0:
        result = None
    return (result, answer)


def smallest(min_factor, max_factor):
    if min_factor > max_factor:
        raise ValueError("min must be <= max")
    result = 0
    answer = []
    for number_a in range(min_factor, max_factor+1):
        was_smaller = False
        for number_b in range(min_factor, max_factor+1):
            if number_a * number_b <= result or result == 0:
                was_smaller = True
                test_value = str(number_a * number_b)
                if test_value == test_value[::-1]:
                    if number_a * number_b < result:
                        answer = []
                    result = int(test_value)
                    answer.append([number_a, number_b])
        if not was_smaller:
            break
    if result == 0:
        result = None
    return (result, answer)
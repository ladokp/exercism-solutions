import math


def triplets_with_sum(number):
    n = float(number)
    output = []
    for c in range(int(n / 2) - 1, int((math.sqrt(2) - 1) * n), -1):
        d = math.sqrt(c**2 - n**2 + 2 * n * c)
        if d == int(d):
            output.append([int((n - c - d) / 2), int((n - c + d) / 2), c])
    return output

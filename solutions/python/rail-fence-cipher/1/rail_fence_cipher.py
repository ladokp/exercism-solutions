from itertools import cycle


def rail_pattern(n):
    r = list(range(n))
    return cycle(r + r[-2:0:-1])


def encode(plaintext, rails):
    pattern = rail_pattern(rails)
    return "".join(sorted(plaintext, key=lambda i: next(pattern)))


def decode(ciphertext, rails):
    pattern = rail_pattern(rails)
    indexes = sorted(range(len(ciphertext)), key=lambda i: next(pattern))
    result = [""] * len(ciphertext)
    for i, c in zip(indexes, ciphertext):
        result[i] = c
    return "".join(result)

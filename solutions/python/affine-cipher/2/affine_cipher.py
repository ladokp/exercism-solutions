from string import ascii_lowercase

ALPHABET = ascii_lowercase
TO_REMOVE = (" ", ".", ",")


def hcf_check(x, y):
    """Calculate the highest common factor (HCF) of two integers x and y using recursion."""
    if y == 0:
        return x
    return hcf_check(y, x % y)


def mmi(a):
    """Find the modular multiplicative inverse of integer a under modulo 26."""
    for x in range(1, 26):
        if (a * x) % 26 == 1:
            return x


def encode(plain_text, a, b):
    """Encode the given plain_text using the affine cipher formula defined by keys a and b."""
    if hcf_check(26, a) != 1:
        raise ValueError("a and m must be coprime.")
    for character in TO_REMOVE:
        plain_text = plain_text.replace(character, "")

    combined = ""

    for i in plain_text.lower():
        if i in ALPHABET:
            ind = (a * ALPHABET.index(i) + b) % 26
            combined += ALPHABET[ind]
        else:
            combined += i

    combined_split = [combined[k : k + 5] for k in range(0, len(combined), 5)]
    return " ".join(combined_split)


def decode(ciphered_text, a, b):
    """Decode the given ciphered_text using the affine cipher formula defined by keys a and b."""
    if hcf_check(26, a) != 1:
        raise ValueError("a and m must be coprime.")
    ciphered_text = ciphered_text.replace(" ", "")
    result = ""
    for character in ciphered_text:
        if character in ALPHABET:
            ind = ((mmi(a)) * (ALPHABET.index(character) - b)) % 26
            result += ALPHABET[ind]
        else:
            result += character
    return result

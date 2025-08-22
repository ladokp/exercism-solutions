from string import ascii_lowercase
from typing import List

ALPHABET = ascii_lowercase
TO_REMOVE = (" ", ".", ",")


def hcf_check(x: int, y: int) -> int:
    """Calculate the highest common factor (HCF) of two integers x and y using recursion.

    Args:
        x (int): First integer.
        y (int): Second integer.

    Returns:
        int: The highest common factor of x and y.
    """
    if y == 0:
        return x
    return hcf_check(y, x % y)


def mmi(a: int) -> int:
    """Find the modular multiplicative inverse of integer a under modulo 26.

    Args:
        a (int): The integer to find the modular inverse for.

    Returns:
        int: The modular multiplicative inverse of a.
    """
    for x in range(1, 26):
        if (a * x) % 26 == 1:
            return x


def encode(plain_text: str, a: int, b: int) -> str:
    """Encode the given plain_text using the affine cipher formula defined by keys a and b.

    Args:
        plain_text (str): The text to encode.
        a (int): The first key for encoding.
        b (int): The second key for encoding.

    Returns:
        str: The encoded text.

    Raises:
        ValueError: If a and m are not coprime.
    """
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

    combined_split: List[str] = [
        combined[k : k + 5] for k in range(0, len(combined), 5)
    ]
    return " ".join(combined_split)


def decode(ciphered_text: str, a: int, b: int) -> str:
    """Decode the given ciphered_text using the affine cipher formula defined by keys a and b.

    Args:
        ciphered_text (str): The text to decode.
        a (int): The first key for decoding.
        b (int): The second key for decoding.

    Returns:
        str: The decoded text.

    Raises:
        ValueError: If a and m are not coprime.
    """
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

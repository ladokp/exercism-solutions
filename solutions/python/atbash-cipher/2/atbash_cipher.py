from string import ascii_letters

ATBASH_CIPHER = str.maketrans(ascii_letters, ascii_letters.lower()[::-1])


def encode(plain_text):
    """
    Encodes the given plain text using the Atbash cipher.

    The Atbash cipher is a monoalphabetic substitution cipher where each letter of
    the alphabet is replaced by its reverse counterpart. This function first decodes
    the plain text using the Atbash cipher logic and then formats the encoded text into
    groups of five characters separated by spaces.

    Args:
        plain_text (str): The text to be encoded.

    Returns:
        str: The encoded text formatted in groups of five characters.
    """
    encoded = decode(plain_text)
    return " ".join(
        encoded[index : index + 5] for index in range(0, len(encoded), 5)
    )


def decode(ciphered_text):
    """
    Decodes the given ciphered text using the Atbash cipher.

    This function removes any non-alphanumeric characters from the input and then
    translates the remaining characters using the Atbash cipher mapping.

    Args:
        ciphered_text (str): The text to be decoded.

    Returns:
        str: The decoded text after applying the Atbash cipher.
    """
    return "".join(c for c in ciphered_text if c.isalnum()).translate(
        ATBASH_CIPHER
    )

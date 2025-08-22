from string import ascii_letters

ATBASH_CIPHER = str.maketrans(ascii_letters, ascii_letters.lower()[::-1])


def encode(plain_text):
    encoded = decode(plain_text)
    return " ".join(
        encoded[index : index + 5] for index in range(0, len(encoded), 5)
    )


def decode(ciphered_text):
    return "".join(c for c in ciphered_text if c.isalnum()).translate(ATBASH_CIPHER)

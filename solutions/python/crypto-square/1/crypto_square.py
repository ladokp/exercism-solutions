import math
import string


def cipher_text(plain_text):
    def sanitize_text(text_to_sanitize):
        return (
            text_to_sanitize.lower()
            .strip()
            .translate(
                str.maketrans("", "", string.punctuation + string.whitespace)
            )
        )

    text = sanitize_text(plain_text)

    if not text:
        return ""

    columns = math.ceil(math.sqrt(len(text)))
    rows = math.ceil(len(text) / columns)
    text += " " * ((columns * rows) - len(text))
    return " ".join(text[i::columns] for i in range(columns))

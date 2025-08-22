import re


def translate(text):
    return " ".join(
        re.sub(r"([xy](?=[aeiouy])|s*qu|[bcdfghjklmnpqrstvwz]*)(.*)", r"\2\1", word)
        + "ay"
        for word in text.split()
    )

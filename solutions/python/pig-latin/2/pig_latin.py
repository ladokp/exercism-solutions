import re

REGEX = r"([xy](?=[aeiouy])|s*qu|[bcdfghjklmnpqrstvwz]*)(.*)"


def translate(text):
    return " ".join(re.sub(REGEX, r"\2\1", word) + "ay" for word in text.split())

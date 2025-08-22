import string


def is_pangram(sentence):
    sentence = sentence.lower()
    for character in string.ascii_lowercase:
        if character not in sentence:
            return False
    return True

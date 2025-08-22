import string


def rotate(text, key):
    upper_shifted = string.ascii_uppercase[key:] + string.ascii_uppercase[:key]
    lower_shifted = string.ascii_lowercase[key:] + string.ascii_lowercase[:key]
    translation = str.maketrans(
        string.ascii_uppercase + string.ascii_lowercase, upper_shifted + lower_shifted
    )
    return text.translate(translation)

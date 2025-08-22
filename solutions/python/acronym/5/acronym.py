def abbreviate(words):
    """Generate an abbreviation from a string of words.

    Args:
        words (str): A string containing words to abbreviate.

    Returns:
        str: The abbreviation formed by the first letter of each word in uppercase.
    """
    words = words.replace("-", " ").replace("_", "").split(" ")
    return "".join(word[0].upper() for word in words if word)

def response(hey_bob: str) -> str:
    """
    Determines the appropriate response to a given input.

    Args:
        hey_bob (str): The input string to evaluate.

    Returns:
        str: The response based on the input characteristics.
    """
    is_empty = not (hey_bob := hey_bob.strip())
    is_question = not is_empty and hey_bob.endswith("?")
    is_yelling = not is_empty and hey_bob.isupper()

    match (is_empty, is_question, is_yelling):
        case (True, _, _):
            return "Fine. Be that way!"
        case (_, True, True):
            return "Calm down, I know what I'm doing!"
        case (_, True, False):
            return "Sure."
        case (_, False, True):
            return "Whoa, chill out!"
    return "Whatever."

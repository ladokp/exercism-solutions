def find_anagrams(word, candidates):
    """Finds all anagrams of a given word from a list of candidates.

    An anagram is a word formed by rearranging the letters of another, using all the original letters exactly once.

    Args:
        word (str): The word for which anagrams need to be found.
        candidates (list of str): A list of candidate words to check against the provided word.

    Yields:
        str: Each anagram found in the candidates list.

    Note:
        The comparison is case insensitive, and the original word is excluded from the results.
    """
    return (
        candidate
        for candidate in candidates
        if candidate.casefold() != word.casefold()
        and sorted(candidate.casefold()) == sorted(word.casefold())
    )

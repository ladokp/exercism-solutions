def find_anagrams(word, candidates):
    return [
        candidate
        for candidate in candidates
        if candidate.casefold() != word.casefold()
        and sorted(candidate.casefold()) == sorted(word.casefold())
    ]

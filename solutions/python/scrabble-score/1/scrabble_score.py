def score(word):
    scores = {
        "AEIOULNRST": 1,
        "DG": 2,
        "BCMP": 3,
        "FHVWY": 4,
        "K": 5,
        "JX": 8,
        "QZ": 10,
    }
    for key, value in tuple(scores.items()):
        del scores[key]
        scores.update({character: value for character in key})
    return sum([scores[character] for character in word.upper()])

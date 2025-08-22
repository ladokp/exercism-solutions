def abbreviate(words):
    return "".join(
        word[0]
        for word in words.replace("-", " ").replace("_", "").upper().split(" ")
        if word
    )

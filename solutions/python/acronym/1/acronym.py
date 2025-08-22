def abbreviate(words):
    words = words.replace("-", " ").replace("_", "").upper()
    return "".join(word[0] for word in words.split(" ") if word)

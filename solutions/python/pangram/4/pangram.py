def is_pangram(sentence):
    return set("abcdefghijklmnopqrstuvwxyz") - set(sentence.lower()) == set([])

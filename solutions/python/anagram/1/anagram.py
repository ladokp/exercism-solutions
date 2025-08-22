def find_anagrams(word, candidates):
    anagram_list = []
    word_list = word.split()
    word_list.sort()
    for candidate in candidates:
        candidate_list = candidate.split()
        candidate_list.sort()
        if word_list == candidate_list:
            anagram_list.append(candidate)
    return anagram_list

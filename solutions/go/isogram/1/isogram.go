package isogram

import "strings"

func IsIsogram(word string) bool {
	var uniqueLetters = ""
	word = strings.ToLower(word)
	word = strings.ReplaceAll(word, "-", "")
	word = strings.ReplaceAll(word, " ", "")

	var sb strings.Builder
	sb.WriteString(uniqueLetters)

	for _, rune := range word {
		if !strings.ContainsRune(uniqueLetters, rune) {
			sb.WriteRune(rune)
			uniqueLetters = sb.String()
		} else {
			return false
		}
	}

	return true
}

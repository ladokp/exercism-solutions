package pangram

import "strings"

func IsPangram(input string) bool {
	input = strings.ToLower(input)
	for rune := 'a'; rune <= 'z'; rune++ {
		if !strings.ContainsRune(input, rune) {
			return false
		}
	}
	return true
}

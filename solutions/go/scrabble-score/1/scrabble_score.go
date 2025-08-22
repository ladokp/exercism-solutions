package scrabble

import "strings"

func Score(word string) int {
	var count = 0
	word = strings.ToLower(word)
	for _, rune := range word {
		switch {
		case strings.ContainsRune("aeioulnrst", rune):
			count += 1
		case strings.ContainsRune("dg", rune):
			count += 2
		case strings.ContainsRune("bcmp", rune):
			count += 3
		case strings.ContainsRune("fhvwy", rune):
			count += 4
		case strings.ContainsRune("k", rune):
			count += 5
		case strings.ContainsRune("jx", rune):
			count += 8
		case strings.ContainsRune("qz", rune):
			count += 10
		}
	}
	return count
}

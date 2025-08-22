package pangram

import "strings"

func IsPangram(input string) bool {
	for _, character := range "abcdefghijklmnopqrstuvwxyz" {
		if !strings.Contains(strings.ToLower(input), string(character)) {
			return false
		}
	}
	return true
}

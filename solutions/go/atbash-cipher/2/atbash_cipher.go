package atbash

import (
	"strings"
	"unicode"
)

const clusterSize = 5

func Atbash(plain string) string {
	var letterCount int8
	cipherSlice := make([]rune, 0, len(plain))
	for _, character := range strings.ToLower(plain) {
		if unicode.IsLetter(character) {
			character = 'z' - character%'a'
		}
		if unicode.IsLetter(character) || unicode.IsNumber(character) {
			cipherSlice = append(cipherSlice, character)
			letterCount++
			if letterCount%clusterSize == 0 {
				cipherSlice = append(cipherSlice, ' ')
			}
		}
	}
	return strings.TrimSpace(string(cipherSlice))
}

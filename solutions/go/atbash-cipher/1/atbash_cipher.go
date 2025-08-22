package atbash

import (
	"strings"
	"unicode"
)

func Atbash(plain string) (ciphered string) {
	plain = strings.ToLower(plain)
	var characterCount int8
	for _, character := range plain {
		if unicode.IsLetter(character) {
			character = 'z' - character%'a'
		}
		if unicode.IsLetter(character) || unicode.IsNumber(character) {
			ciphered += string(character)
			characterCount++
			if characterCount%5 == 0 {
				ciphered += " "
			}
		}
	}
	return strings.TrimSpace(ciphered)
}

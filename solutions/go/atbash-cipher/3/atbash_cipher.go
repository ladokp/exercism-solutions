package atbash

import (
	"bytes"
	"strings"
	"unicode"
)

const clusterSize = 5

func Atbash(plain string) string {
	var letterCount int8
	ciphered := bytes.Buffer{}
	for _, character := range strings.ToLower(plain) {
		if unicode.IsLetter(character) {
			character = 'z' - character + 'a'
		}
		if unicode.IsLetter(character) || unicode.IsNumber(character) {
			ciphered.WriteRune(character)
			letterCount++
			if letterCount%clusterSize == 0 {
				ciphered.WriteByte(' ')
			}
		}
	}
	return strings.TrimSpace(ciphered.String())
}

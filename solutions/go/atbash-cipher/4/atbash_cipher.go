package atbash

import (
	"bytes"
	"strings"
)

const (
	clusterSize = 5
	encryption  = "abcdefghijklmnopqrstuvwxyz"
)

func Atbash(plain string) string {
	var letterCount int8
	ciphered := bytes.Buffer{}
	for _, character := range strings.ToLower(plain) {
		if 'a' <= character && character <= 'z' || '0' <= character && character <= '9' {
			if 'a' <= character && character <= 'z' {
				ciphered.WriteByte(encryption['z'-character])
			} else {
				ciphered.WriteRune(character)
			}
			letterCount++
			if letterCount%clusterSize == 0 {
				ciphered.WriteByte(' ')
			}
		}
	}
	return strings.TrimSpace(ciphered.String())
}

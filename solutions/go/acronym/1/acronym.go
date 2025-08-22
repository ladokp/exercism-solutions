package acronym

import (
	"bytes"
	"strings"
)

func Abbreviate(sentence string) string {
	sentence = strings.ReplaceAll(sentence, "-", " ")
	resultBuffer := bytes.Buffer{}
	for _, word := range strings.Split(sentence, " ") {
		if len(word) > 0 {
			character := word[0]
			if character == '_' {
				character = word[1]
			} else if character >= 'a' && character <= 'z' {
				character -= 32
			}
			resultBuffer.WriteByte(character)
		}
	}
	return resultBuffer.String()
}

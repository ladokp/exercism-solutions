package cipher

import (
	"bytes"
	"strings"
)

type Shift struct{ encodeKey, decodeKey []rune }
type Caesar Shift
type Vigenere Shift

func NewCaesar() Cipher {
	return Caesar{[]rune{3}, []rune{-3}}
}
func NewShift(d int) Cipher {
	if d <= -25 || d > 25 || d == 0 {
		return nil
	}
	offset := rune(d)
	return Shift{[]rune{offset}, []rune{-offset}}
}
func NewVigenere(key string) Cipher {
	if len(key) < 3 {
		return nil
	}
	var encodeKey, decodeKey []rune
	for _, r := range key {
		if r < 'a' || r > 'z' {
			return nil
		}
		offset := r - 'a'
		encodeKey = append(encodeKey, offset)
		decodeKey = append(decodeKey, -offset)
	}
	return Vigenere{encodeKey, decodeKey}
}
func (c Caesar) Encode(m string) string   { return shift(m, c.encodeKey) }
func (c Caesar) Decode(m string) string   { return shift(m, c.decodeKey) }
func (s Shift) Encode(m string) string    { return shift(m, s.encodeKey) }
func (s Shift) Decode(m string) string    { return shift(m, s.decodeKey) }
func (v Vigenere) Encode(m string) string { return shift(m, v.encodeKey) }
func (v Vigenere) Decode(m string) string { return shift(m, v.decodeKey) }
func shift(sentence string, offset []rune) string {
	cipher := bytes.Buffer{}
	offsetIndex, length := 0, len(offset)
	for _, character := range strings.ToLower(sentence) {
		var shiftedCharacter rune = -1
		if character >= 'a' && character <= 'z' {
			shiftedCharacter = 'a' + ((character-'a')+offset[offsetIndex]+26)%26
			if offsetIndex++; offsetIndex >= length {
				offsetIndex = 0
			}
			cipher.WriteRune(shiftedCharacter)
		}
	}
	return cipher.String()
}

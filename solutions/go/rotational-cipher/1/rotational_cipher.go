package rotationalcipher

func RotationalCipher(plain string, shiftKey int) (cipher string) {
	for _, character := range plain {
		if 65 <= character && character < 97 {
			cipheredRune := rune(int(character)-65+shiftKey) % 26
			cipher += string(cipheredRune + 'A')
		} else if 97 <= character && character < 129 {
			cipheredRune := rune(int(character)-97+shiftKey) % 26
			cipher += string(cipheredRune + 'a')
		} else {
			cipher += string(character)
		}
	}
	return
}

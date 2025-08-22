package rotationalcipher

func RotationalCipher(plain string, shiftKey int) (cipher string) {
	for _, character := range plain {
		if 'A' <= character && character <= 'Z' {
			cipher += string(rune(int(character)-'A'+shiftKey)%26 + 'A')
		} else if 'a' <= character && character <= 'z' {
			cipher += string(rune(int(character)-'a'+shiftKey)%26 + 'a')
		} else {
			cipher += string(character)
		}
	}
	return
}

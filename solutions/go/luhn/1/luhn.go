package luhn

import (
	"strconv"
	"strings"
)

func Reverse(s string) string {
	n := len(s)
	runes := make([]rune, n)
	for _, rune := range s {
		n--
		runes[n] = rune
	}
	return string(runes[n:])
}

func Valid(id string) bool {
	const validCharacters = "0123456789"
	var checkSum = 0
	id = strings.ReplaceAll(id, " ", "")
	id = Reverse(id)

	if len(id) == 1 {
		return false
	}

	for index, rune := range id {
		if !strings.ContainsRune(validCharacters, rune) {
			return false
		}
		var digit, _ = strconv.Atoi(string(rune))
		if (index+1)%2 == 0 {
			var doubledDigit = digit * 2
			if doubledDigit > 9 {
				doubledDigit -= 9
			}
			checkSum += doubledDigit
		} else {
			checkSum += digit
		}
	}

	return checkSum%10 == 0
}

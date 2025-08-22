package isbn

import (
	"strings"
	"unicode"
)

func IsValidISBN(isbn string) bool {
	isbn = strings.ReplaceAll(isbn, "-", "")
	if len(isbn) != 10 {
		return false
	}
	result := 0

	for index, value := range isbn {
		if index < 9 && unicode.IsLetter(value) {
			return false
		}

		digit := int(value - '0')
		if value == 'X' {
			digit = 10
		}
		result += digit * (10 - index)
	}
	return result%11 == 0
}

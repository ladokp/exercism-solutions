package phonenumber

import (
	"bytes"
	"errors"
	"fmt"
)

func Number(phoneNumber string) (string, error) {
	cleanedPhoneNumberBuffer := bytes.Buffer{}
	for _, character := range phoneNumber {
		if character >= '0' && character <= '9' {
			cleanedPhoneNumberBuffer.WriteRune(character)
		}
	}
	cleanedPhoneNumber := cleanedPhoneNumberBuffer.String()
	if len(cleanedPhoneNumber) == 11 && cleanedPhoneNumber[0] == '1' {
		cleanedPhoneNumber = cleanedPhoneNumber[1:]
	}
	if len(cleanedPhoneNumber) != 10 || cleanedPhoneNumber[0] < '2' || cleanedPhoneNumber[3] <= '1' {
		return "", errors.New("invalid input")
	}
	return cleanedPhoneNumber, nil
}

func AreaCode(phoneNumber string) (string, error) {
	cleanedPhoneNumber, error_ := Number(phoneNumber)
	if error_ != nil {
		return "", error_
	}
	return cleanedPhoneNumber[:3], nil
}

func Format(phoneNumber string) (string, error) {
	cleanedPhoneNumber, error_ := Number(phoneNumber)
	if error_ != nil {
		return "", error_
	}
	return fmt.Sprintf("(%s) %s-%s", cleanedPhoneNumber[:3], cleanedPhoneNumber[3:6], cleanedPhoneNumber[6:]), nil
}

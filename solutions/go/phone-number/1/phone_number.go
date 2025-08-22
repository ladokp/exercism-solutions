package phonenumber

import (
	"bytes"
	"errors"
	"fmt"
	"strings"
	"unicode"
)

func Number(phoneNumber string) (string, error) {
	cleanedPhoneNumberBuffer := bytes.Buffer{}
	for _, character := range phoneNumber {
		if !unicode.IsDigit(character) && !strings.Contains("+(. -)", string(character)) {
			return "", errors.New("invalid phone number")
		}
		if unicode.IsDigit(character) {
			cleanedPhoneNumberBuffer.WriteRune(character)
		}
	}
	cleanedPhoneNumber := cleanedPhoneNumberBuffer.String()
	length := len(cleanedPhoneNumber)
	if length < 10 {
		return "", errors.New("must not be fewer than 10 digits")
	}
	if length > 11 {
		return "", errors.New("must not be greater than 11 digits")
	}
	if length == 11 && cleanedPhoneNumber[0] != '1' {
		return "", errors.New("11 digits must start with 1")
	}
	if length == 11 {
		cleanedPhoneNumber = cleanedPhoneNumber[1:]
	}
	if cleanedPhoneNumber[0] == '0' || cleanedPhoneNumber[0] == '1' {
		return "", errors.New("area code must not start with 0")
	}
	if cleanedPhoneNumber[3] == '0' || cleanedPhoneNumber[3] == '1' {
		return "", errors.New("exchange code must not start with 1")
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

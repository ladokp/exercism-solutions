package prime

import (
	"errors"
)

func Nth(number int) (int, error) {
	if number <= 0 {
		return 0, errors.New("nth prime number cannot be computed")
	}
	primeCount := 0
	for count := 2; ; count++ {
		if IsPrime(count) {
			primeCount++
		}
		if primeCount == number {
			return count, nil
		}
	}
}
func IsPrime(number int) bool {
	for count := 2; count < number; count++ {
		if number%count == 0 && number != count {
			return false
		}
	}
	return true
}

package allyourbase

import "errors"

func ConvertToBase(base int, digits []int, outputBase int) ([]int, error) {
	if base < 2 {
		return []int{0}, errors.New("input base must be >= 2")
	}
	if outputBase < 2 {
		return []int{0}, errors.New("output base must be >= 2")
	}
	conversion := make([]int, 0)
	sum := 0
	for _, digit := range digits {
		if digit >= base || digit < 0 {
			return nil, errors.New("all digits must satisfy 0 <= d < input base")
		}
		sum = sum*base + digit
	}
	if sum == 0 {
		return []int{0}, nil
	}
	for ; sum > 0; sum /= outputBase {
		conversion = append([]int{sum % outputBase}, conversion...)
	}
	return conversion, nil
}

package lsproduct

import (
	"errors"
)

func LargestSeriesProduct(digits string, span int) (int64, error) {
	if span > len(digits) || span < 0 {
		return 0, errors.New("incorrect span")
	}

	digitsBytes := make([]byte, len(digits))
	for index, value := range digits {
		if value < '0' || value > '9' {
			return 0, errors.New("not a digit")
		}
		digitsBytes[index] = byte(value - '0')
	}
	max := int64(0)
	for index := 0; index < len(digitsBytes)-span+1; index++ {
		value := int64(1)
		for number := index; number < index+span; number++ {
			value = value * int64(digitsBytes[number])
		}
		if value > max {
			max = value
		}
	}
	return max, nil
}

package grains

import "errors"

func Square(number int) (uint64, error) {
	if number < 1 || number > 64 {
		return 0, errors.New("wrong field number")
	}

	var currentSum uint64 = 0
	for i := 1; i <= number; i += 1 {
		if i == 1 {
			currentSum = 1
		} else {
			currentSum *= 2
		}
	}
	return currentSum, nil
}

func Total() uint64 {
	var total uint64 = 0
	for i := 1; i <= 64; i += 1 {
		var grains, _ = Square(i)
		total += grains
	}
	return total
}

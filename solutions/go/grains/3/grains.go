package grains

import (
	"errors"
	"math"
)

func Square(number int) (uint64, error) {
	if number < 1 || number > 64 {
		return 0, errors.New("wrong field number")
	}

	return 1 << uint(number-1), nil
}

func Total() uint64 {
	return math.MaxUint64
}

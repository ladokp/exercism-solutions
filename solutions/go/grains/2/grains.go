package grains

import (
	"errors"
	"strconv"
	"strings"
)

func convertBinaryString(binaryString string) (uint64, error) {
	if num, err := strconv.ParseUint(binaryString, 2, 64); err != nil {
		return 0, err
	} else {
		return num, nil
	}
}

func Square(number int) (uint64, error) {
	if number < 1 || number > 64 {
		return 0, errors.New("wrong field number")
	}

	var grainsBinaryString = "1" + strings.Repeat("0", number-1)
	return convertBinaryString(grainsBinaryString)
}

func Total() uint64 {
	var grainsBinaryString = strings.Repeat("1", 64)
	grains, _ := convertBinaryString(grainsBinaryString)
	return grains
}

package armstrong

import (
	"math"
	"strconv"
)

func IsNumber(number int) bool {
	numberString := strconv.FormatInt(int64(number), 10)
	numberLength := float64(len(numberString))
	var calculatedNumber float64
	for _, digit := range numberString {
		digitFloat := float64(int(digit) - '0')
		calculatedNumber += math.Pow(digitFloat, numberLength)
	}
	return int(calculatedNumber) == number
}

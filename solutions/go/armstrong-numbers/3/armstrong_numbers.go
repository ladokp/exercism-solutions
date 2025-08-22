package armstrong

import (
	"math"
)

func IsNumber(number int) bool {
	var (
		digits           = float64(int(math.Log10(float64(number)) + 1))
		calculatedNumber float64
	)
	for digitBase := number; digitBase > 0; {
		calculatedNumber += math.Pow(float64(digitBase%10), digits)
		digitBase /= 10
	}
	return int(calculatedNumber) == number
}

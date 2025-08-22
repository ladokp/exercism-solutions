package perfect

import (
	"errors"
	"math"
)

type Classification uint8

const (
	ClassificationPerfect Classification = iota
	ClassificationAbundant
	ClassificationDeficient
)

var ErrOnlyPositive = errors.New("only positive numbers possible")

func Classify(number int64) (Classification, error) {
	if number < 1 {
		return ClassificationDeficient, ErrOnlyPositive
	}

	var aliquotSum int64
	for divisor := int64(1); divisor <= int64(math.Floor(float64(number)/2)); divisor++ {
		if number%divisor == 0 {
			aliquotSum += divisor
		}
	}
	if aliquotSum == number {
		return ClassificationPerfect, nil
	}
	if aliquotSum > number {
		return ClassificationAbundant, nil
	}
	return ClassificationDeficient, nil
}

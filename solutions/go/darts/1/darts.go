package darts

import "math"

func Score(x, y float64) int {
	distance := math.Sqrt(math.Pow(x, 2) + math.Pow(y, 2))
	if distance <= 1 {
		return 10
	} else if distance <= 5 {
		return 5
	} else if distance <= 10 {
		return 1
	}
	return 0
}

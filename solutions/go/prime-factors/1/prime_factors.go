package prime

func Factors(number int64) (result []int64) {
	factor := int64(2)
	for number > 1 {
		if number%factor == 0 {
			result = append(result, factor)
			number /= factor
		} else {
			factor += 1
		}
	}
	return
}

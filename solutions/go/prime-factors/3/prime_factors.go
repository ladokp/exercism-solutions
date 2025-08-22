package prime

func Factors(number int64) (factors []int64) {
	for factor := int64(2); number%factor == 0; {
		factors = append(factors, factor)
		number /= factor
	}
	for factor := int64(3); factor*factor <= number; factor += 2 {
		for number%factor == 0 {
			factors = append(factors, factor)
			number /= factor
		}
	}
	if number > 2 {
		factors = append(factors, number)
	}
	return
}

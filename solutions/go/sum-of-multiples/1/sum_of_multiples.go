package summultiples

func SumMultiples(limit int, divisors ...int) (sum int) {
	for number := 1; number < limit; number++ {
		for _, divisor := range divisors {
			if divisor != 0 && number%divisor == 0 {
				sum += number
				break
			}
		}
	}
	return
}

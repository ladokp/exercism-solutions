package sieve

func Sieve(number int) []int {
	numbers := make([]bool, number+1)
	var primes []int

	for index := 2; index <= number; index++ {
		marked := numbers[index]
		if !marked {
			primes = append(primes, index)
			for secondIndex := 1; ; secondIndex++ {
				currentIndex := index * secondIndex
				if currentIndex > number {
					break
				}
				numbers[currentIndex] = true
			}
		}
	}
	return primes
}

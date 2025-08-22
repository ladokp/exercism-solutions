package diffsquares

func SquareOfSum(n int) int {
	var sum = 0
	for i := 1; i <= n; i++ {
		sum += i
	}
	return sum * sum
}

func SumOfSquares(n int) int {
	var sumOfSquares = 0
	for i := 1; i <= n; i++ {
		sumOfSquares += i * i
	}
	return sumOfSquares
}

func Difference(n int) int {
	return SquareOfSum(n) - SumOfSquares(n)
}

package pythagorean

type Triplet [3]int

func isPythagoreanTriangle(a, b, c int) bool {
	return a*a+b*b == c*c
}

// Range generates list of all Pythagorean triplets with side lengths
// in the provided range.
func Range(min, max int) (result []Triplet) {
	for a := min; a <= max; a++ {
		for b := a; b <= max; b++ {
			for c := b; c <= max; c++ {
				if isPythagoreanTriangle(a, b, c) {
					result = append(result, Triplet{a, b, c})
				}
			}
		}
	}
	return
}

// Sum returns a list of all Pythagorean triplets with a certain perimeter.
func Sum(sum int) (result []Triplet) {
	upperBorder := sum / 2
	for a := 1; a <= upperBorder; a++ {
		for b := a; b <= upperBorder; b++ {
			if c := sum - a - b; isPythagoreanTriangle(a, b, c) {
				result = append(result, Triplet{a, b, c})
			}
		}
	}
	return
}

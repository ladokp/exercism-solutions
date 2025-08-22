package complexnumbers

import "math"

type Number struct {
	real      float64
	imaginary float64
}

func (number Number) Real() float64 {
	return number.real
}

func (number Number) Imaginary() float64 {
	return number.imaginary
}

func (number Number) Add(n2 Number) Number {
	return Number{
		real:      number.real + n2.real,
		imaginary: number.imaginary + n2.imaginary,
	}
}

func (number Number) Subtract(n2 Number) Number {
	return Number{
		real:      number.real - n2.real,
		imaginary: number.imaginary - n2.imaginary,
	}
}

func (number Number) Multiply(n2 Number) Number {
	return Number{
		real:      number.real*n2.real - number.imaginary*n2.imaginary,
		imaginary: number.imaginary*n2.real + number.real*n2.imaginary,
	}
}

func (number Number) Times(factor float64) Number {
	return Number{
		real:      factor * number.real,
		imaginary: factor * number.imaginary,
	}
}

func (number Number) Divide(n2 Number) Number {
	divisor := n2.real*n2.real + n2.imaginary*n2.imaginary
	return Number{
		real:      (number.real*n2.real + number.imaginary*n2.imaginary) / divisor,
		imaginary: (number.imaginary*n2.real - number.real*n2.imaginary) / divisor,
	}
}

func (number Number) Conjugate() Number {
	return Number{
		real:      number.real,
		imaginary: -number.imaginary,
	}
}

func (number Number) Abs() float64 {
	return math.Sqrt(number.real*number.real + number.imaginary*number.imaginary)
}

func (number Number) Exp() Number {
	factor := math.Exp(number.real)
	return Number{
		real:      factor * math.Cos(number.imaginary),
		imaginary: factor * math.Sin(number.imaginary),
	}
}

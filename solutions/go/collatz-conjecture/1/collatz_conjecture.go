package collatzconjecture

import (
	"errors"
)

func CollatzConjecture(n int) (int, error) {
	if n <= 0 {
		return 0, errors.New("number cannot lower than one")
	}
	result := 0
	if n == 1 {
		return result, nil
	} else if n%2 == 0 {
		result, error := CollatzConjecture(n / 2)
		if error != nil {
			return 0, error
		}
		return 1 + result, nil
	} else {
		result, error := CollatzConjecture(3*n + 1)
		if error != nil {
			return 0, error
		}
		return 1 + result, nil
	}
}

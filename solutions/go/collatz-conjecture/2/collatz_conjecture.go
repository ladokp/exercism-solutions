package collatzconjecture

import (
	"errors"
)

func CollatzConjecture(n int) (int, error) {
	if n <= 0 {
		return -1, errors.New("number cannot be lower than one")
	}
	if n == 1 {
		return 0, nil
	} else if n%2 == 0 {
		result, _ := CollatzConjecture(n / 2)
		return 1 + result, nil
	} else {
		result, _ := CollatzConjecture(3*n + 1)
		return 1 + result, nil
	}
}

package thefarm

import "fmt"
import "errors"

// See types.go for the types defined for this exercise.

// SillyNephewError type describes the nephew's inability to count.
type SillyNephewError struct {
	cows int
}

func (e *SillyNephewError) Error() string {
	return fmt.Sprintf("silly nephew, there cannot be %d cows", e.cows)
}

// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(weightFodder WeightFodder, cows int) (float64, error) {
	fodder, err := weightFodder.FodderAmount()
	switch {
	case fodder < 0 && err != nil && err != ErrScaleMalfunction:
		return 0, errors.New("non-scale error")
	case fodder < 0:
		return 0, errors.New("negative fodder")
	case cows == 0:
		return 0, errors.New("division by zero")
	case err == ErrScaleMalfunction:
		fodder *= 2
	case err != nil:
		return 0, err
	case cows < 0:
		return 0, &SillyNephewError{cows: cows}
	}
	return fodder / float64(cows), nil
}

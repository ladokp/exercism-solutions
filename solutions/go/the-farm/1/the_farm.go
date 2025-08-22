package thefarm

import "fmt"
import "errors"

// See types.go for the types defined for this exercise.

// TODO: Define the SillyNephewError type here.
type SillyNephewError struct {
    cows int
}

func (e *SillyNephewError) Error() string {
  return fmt.Sprintf("silly nephew, there cannot be %d cows", e.cows)
}

// DivideFood computes the fodder amount per cow for the given cows.
func DivideFood(weightFodder WeightFodder, cows int) (float64, error) {
	if cows == 0 {
        return 0.0, errors.New("Division by zero")
    } else if cows < 0 {
        return 0.0, &SillyNephewError{cows: cows,}
    }
    
    fodderAmount, err := weightFodder.FodderAmount()
    if fodderAmount < 0 {
        return 0.0, errors.New("Negative fodder")
    } else if err == ErrScaleMalfunction && fodderAmount > 0 {
		return (2 * fodderAmount) / float64(cows), nil
	} else if err != nil {
    	return 0.0, err
    }
	return fodderAmount / float64(cows), nil
}

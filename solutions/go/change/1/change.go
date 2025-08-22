package change

import (
	"errors"
)

func Change(coins []int, amount int) ([]int, error) {
	if amount < 0 {
		return nil, errors.New("amount below zero is not allowed")
	}

	amounts := make([][]int, amount+1)
	amounts[0] = []int{}

	for index := range amounts {
		for _, coin := range coins {
			if index-coin >= 0 && amounts[index-coin] != nil && (amounts[index] == nil || len(amounts[index-coin])+1 < len(amounts[index])) {
				amounts[index] = append([]int{coin}, amounts[index-coin]...)
			}
		}
	}

	if amounts[amount] == nil {
		return nil, errors.New("no change possible")
	}

	return amounts[amount], nil
}

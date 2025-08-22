package queenattack

import (
	"errors"
)

var errInput = errors.New("incorrect input")

// CanQueenAttack returns if two queens can attack each other
func CanQueenAttack(white, black string) (bool, error) {
	if !validQueen(white) || !validQueen(black) || white == black {
		return false, errInput
	}
	return white[0] == black[0] || white[1] == black[1] || white[0]-black[0] == white[1]-black[1] || white[0]-black[0] == black[1]-white[1], nil
}

func validQueen(queen string) bool {
	return len(queen) == 2 &&
		queen[1] >= byte('1') &&
		queen[1] <= byte('8') &&
		queen[0] >= byte('a') &&
		queen[0] <= byte('h')
}

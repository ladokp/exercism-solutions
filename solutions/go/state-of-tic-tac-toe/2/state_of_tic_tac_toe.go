package stateoftictactoe

import (
	"errors"
)

type State string

const (
	Win     State = "win"
	Ongoing State = "ongoing"
	Draw    State = "draw"
)

func IsValidBoard(board []string) (bool, []int) {
	xs := 0
	os := 0
	marks := 0
	for _, row := range board {
		for _, mark := range row {
			switch mark {
			case 'X':
				xs += 1
			case 'O':
				os += 1
			default:
				continue
			}
			marks += 1
		}
	}
	playerX := IsWinningBoard(board, 'X')
	playerO := IsWinningBoard(board, 'O')
	ret := []int{marks, xs, os}
	if (xs == os+1) || (xs == os) {
		switch {
		case playerO && playerX:
			return false, ret
		case playerO:
			return xs == os, ret
		case playerX && xs != os+1:
			return false, ret
		default:
			return true, ret
		}
	}
	return false, ret
}

func IsWinningBoard(board []string, mark byte) bool {
	for row := 0; row < 3; row += 1 {
		if board[row][0] == board[row][1] &&
			board[row][0] == board[row][2] &&
			board[row][0] == mark {
			return true
		}
	}
	for col := 0; col < 3; col += 1 {
		if board[0][col] == board[1][col] &&
			board[0][col] == board[2][col] &&
			board[0][col] == mark {
			return true
		}
	}
	if board[0][0] == board[1][1] &&
		board[0][0] == board[2][2] &&
		board[0][0] == mark {
		return true
	}
	if board[0][2] == board[1][1] &&
		board[0][2] == board[2][0] &&
		board[0][2] == mark {
		return true
	}
	return false
}

func GameOver(board []string) bool {
	return IsWinningBoard(board, 'X') ||
		IsWinningBoard(board, 'O')
}

func StateOfTicTacToe(board []string) (State, error) {
	valid, res := IsValidBoard(board)
	if !valid {
		return "", errors.New("invalid board")
	}
	if GameOver(board) {
		return Win, nil
	}
	if res[0] == 9 {
		return Draw, nil
	}
	return Ongoing, nil
}

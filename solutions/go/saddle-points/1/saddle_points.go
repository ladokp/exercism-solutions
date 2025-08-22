package matrix

import (
	"strconv"
	"strings"
)

type Matrix [][]int
type Pair struct {
	row, column int
}

func New(s string) (*Matrix, error) {
	var matrix Matrix = make([][]int, 0)
	if s == "" {
		return &matrix, nil
	}
	for _, strRow := range strings.Split(s, "\n") {
		row := []int{}
		for _, strNum := range strings.Split(strRow, " ") {
			if n, err := strconv.Atoi(strNum); err == nil {
				row = append(row, n)
			} else {
				return nil, err
			}
		}
		matrix = append(matrix, row)
	}
	return &matrix, nil
}

func (matrix *Matrix) isSaddlePoint(pair Pair) bool {
	for column := range (*matrix)[0] {
		if (*matrix)[pair.row][column] > (*matrix)[pair.row][pair.column] {
			return false
		}
	}
	for row := range *matrix {
		if (*matrix)[row][pair.column] < (*matrix)[pair.row][pair.column] {
			return false
		}
	}
	return true
}

func (matrix *Matrix) Saddle() []Pair {
	pairs := []Pair{}
	for rowIndex, row := range *matrix {
		for c := range row {
			if matrix.isSaddlePoint(Pair{rowIndex, c}) {
				pairs = append(pairs, Pair{rowIndex + 1, c + 1})
			}
		}
	}
	return pairs
}

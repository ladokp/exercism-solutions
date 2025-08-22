package matrix

import (
	"errors"
	"strconv"
	"strings"
)

// Matrix describes a two-dimensional array of integers
type Matrix [][]int

func New(textMatrix string) (Matrix, error) {
	rows := strings.Split(textMatrix, "\n")
	matrix := make(Matrix, len(rows))
	for index, row := range rows {
		row = strings.TrimSpace(row)
		numbers := strings.Split(row, " ")
		if index > 0 && len(matrix[index-1]) != len(numbers) {
			return nil, errors.New("uneven rows given")
		}
		for _, number := range numbers {
			numberInt, err := strconv.Atoi(number)
			if err != nil {
				return nil, err
			}
			matrix[index] = append(matrix[index], numberInt)
		}
	}
	return matrix, nil
}

func (matrix Matrix) Cols() [][]int {
	columns := make([][]int, len(matrix[0]))
	for columnIndex, _ := range columns {
		columns[columnIndex] = make([]int, len(matrix))
		for rowIndex, _ := range columns[columnIndex] {
			columns[columnIndex][rowIndex] = matrix[rowIndex][columnIndex]
		}
	}
	return columns
}

func (matrix Matrix) Rows() [][]int {
	rows := make([][]int, len(matrix))
	for rowIndex, _ := range rows {
		rows[rowIndex] = make([]int, len(matrix[rowIndex]))
		for columnIndex, _ := range rows[rowIndex] {
			rows[rowIndex][columnIndex] = matrix[rowIndex][columnIndex]
		}
	}
	return rows
}

func (matrix Matrix) Set(rowIndex, columnIndex, value int) bool {
	if rowIndex < 0 || rowIndex >= len(matrix) || columnIndex < 0 || columnIndex >= len(matrix[0]) {
		return false
	}
	matrix[rowIndex][columnIndex] = value
	return true
}

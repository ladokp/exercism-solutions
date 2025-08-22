package spiralmatrix

func SpiralMatrix(size int) [][]int {
	result := [][]int{}
	for row := 0; row < size; row++ {
		result = append(result, make([]int, size))
	}
	for row, column, deltaRow, deltaColumn, index := 0, 0, 0, 1, 1; index <= size*size; index++ {
		result[row][column] = index
		if row+deltaRow < 0 || size <= row+deltaRow || column+deltaColumn < 0 ||
			size <= column+deltaColumn || result[row+deltaRow][column+deltaColumn] != 0 {
			deltaRow, deltaColumn = deltaColumn, -deltaRow
		}
		row, column = row+deltaRow, column+deltaColumn
	}
	return result
}

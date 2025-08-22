package minesweeper

var neigboursShift = [8][2]int{
	{-1, -1}, {-1, 0}, {-1, 1},
	{0, -1}, {0, 1},
	{1, -1}, {1, 0}, {1, 1},
}

// Annotate returns an annotated board
func Annotate(board []string) []string {
	boardLength := len(board)
	if boardLength == 0 {
		return []string{}
	}

	rowLength := len(board[0])
	newBoard := make([]string, boardLength)
	newRow := make([]rune, rowLength)
	for index, row := range board {
		copy(newRow, []rune(row))
		for x, character := range row {
			if character == '*' {
				continue
			} else {
				neighboursCount := 0
				for _, shift := range neigboursShift {
					shiftY := index + shift[0]
					shiftX := x + shift[1]
					if shiftY >= 0 && shiftY < boardLength && shiftX >= 0 && shiftX < rowLength {
						if board[shiftY][shiftX] == '*' {
							neighboursCount++
						}
					}
				}
				if neighboursCount != 0 {
					newRow[x] = rune(neighboursCount + 48)
				}
			}
		}
		newBoard[index] = string(newRow)
	}
	return newBoard
}

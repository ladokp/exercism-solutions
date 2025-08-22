package diamond

import (
	"bytes"
	"errors"
	"strings"
)

// Gen returns a letter diamond for the given character
func Gen(input byte) (string, error) {
	if input < 'A' || input > 'Z' {
		return "", errors.New("character is out of range")
	}

	rowLength := 2*(input-'A') + 1
	rows := make([]string, rowLength)
	for currentCharacter := byte('A'); currentCharacter <= input; currentCharacter++ {
		row := bytes.Repeat([]byte{' '}, int(rowLength))
		columnIndex := input - currentCharacter
		rowIndex := currentCharacter - 'A'

		row[columnIndex] = currentCharacter
		row[rowLength-columnIndex-1] = currentCharacter
		rows[rowIndex] = string(row)
		rows[rowLength-rowIndex-1] = string(row)
	}

	return strings.Join(rows, "\n"), nil
}

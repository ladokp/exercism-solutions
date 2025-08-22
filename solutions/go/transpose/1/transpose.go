package transpose

import "strings"

// Transpose a matrix
func Transpose(rows []string) []string {
	builders := make([]*strings.Builder, 0, 20)
	for index, row := range rows {
		for index2, cell := range []byte(row) {
			if len(builders) <= index2 {
				builders = append(builders, &strings.Builder{})
				builders[index2].Grow(len(rows))
			}
			for builders[index2].Len() < index {
				builders[index2].WriteByte(' ')
			}
			builders[index2].WriteByte(cell)
		}
	}
	transposedRows := make([]string, len(builders))
	for i, row := range builders {
		transposedRows[i] = row.String()
	}
	return transposedRows
}

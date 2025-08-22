package cryptosquare

import (
	"math"
	"regexp"
	"strings"
)

func Encode(text string) string {
	normalizedText := strings.ToLower(regexp.MustCompile(`[^a-zA-Z0-9]`).ReplaceAllString(text, ""))
	rows, columns := dimensions(len(normalizedText))
	var encoded strings.Builder
	for column := 0; column < columns; column++ {
		for row := 0; row < rows; row++ {
			index := row*columns + column
			if index < len(normalizedText) {
				encoded.WriteByte(normalizedText[index])
			} else {
				encoded.WriteString(" ")
			}
		}
		if column != columns-1 {
			encoded.WriteString(" ")
		}
	}
	return encoded.String()
}

func dimensions(length int) (rows, columns int) {
	sqrt := math.Sqrt(float64(length))
	rows = int(math.Floor(sqrt))
	columns = int(math.Ceil(sqrt))
	if rows*columns < length {
		rows++
	}
	return
}

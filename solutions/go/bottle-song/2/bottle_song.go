package bottlesong

import (
	"fmt"
	"strings"
)

var numberToWord = []string{"No", "One", "Two", "Three", "Four", "Five",
	"Six", "Seven", "Eight", "Nine", "Ten"}

func Recite(startBottles, takeDown int) (result []string) {
	stopBottles := startBottles - takeDown
	for i := startBottles; i > stopBottles; i-- {
		if i < startBottles {
			result = append(result, "")
		}
		result = append(result, verse(i)...)
	}
	return
}

func verse(number int) []string {
	line1 := fmt.Sprintf("%s green bottle%s hanging on the wall,",
		numberToWord[number], plural(number))
	line2 := "And if one green bottle should accidentally fall,"
	line3 := fmt.Sprintf("There'll be %s green bottle%s hanging on the wall.",
		strings.ToLower(numberToWord[number-1]), plural(number-1))

	return []string{line1, line1, line2, line3}
}

func plural(number int) (result string) {
	if number != 1 {
		result = "s"
	}
	return
}

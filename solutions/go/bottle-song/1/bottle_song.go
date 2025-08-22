package bottlesong

import (
	"fmt"
	"strings"
)

var numberToWord = []string{"No", "One", "Two", "Three", "Four", "Five",
	"Six", "Seven", "Eight", "Nine", "Ten"}

func Recite(startBottles, takeDown int) []string {
	var res []string
	stopBottles := startBottles - takeDown
	for i := startBottles; i > stopBottles; i-- {
		if i < startBottles {
			res = append(res, "")
		}
		res = append(res, verse(i)...)
	}
	return res
}

func verse(n int) []string {
	line1 := fmt.Sprintf("%s green bottle%s hanging on the wall,",
		numberToWord[n], plural(n))
	line2 := "And if one green bottle should accidentally fall,"
	line3 := fmt.Sprintf("There'll be %s green bottle%s hanging on the wall.",
		strings.ToLower(numberToWord[n-1]), plural(n-1))

	return []string{line1, line1, line2, line3}
}

func plural(n int) (res string) {
	if n != 1 {
		res = "s"
	}
	return res
}

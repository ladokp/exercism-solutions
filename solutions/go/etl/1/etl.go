package etl

import "strings"

func Transform(in map[int][]string) (output map[string]int) {
	output = make(map[string]int)
	for score, letters := range in {
		for _, letter := range letters {
			output[strings.ToLower(letter)] = score
		}
	}
	return
}

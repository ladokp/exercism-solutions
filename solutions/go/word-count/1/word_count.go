package wordcount

import (
	"regexp"
	"strings"
)

type Frequency map[string]int

func WordCount(phrase string) (frequency Frequency) {
	frequency = make(Frequency)
	re := regexp.MustCompile(`\w+('\w+)?`)
	for _, word := range re.FindAllString(strings.ToLower(phrase), -1) {
		frequency[word]++
	}
	return
}

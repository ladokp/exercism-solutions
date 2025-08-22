package scale

import "strings"

var steps = map[rune]int{'m': 1, 'M': 2, 'A': 3}

func Scale(tonic, intervals string) []string {
	if intervals == "" {
		intervals = "mmmmmmmmmmm"
	}
	intervals += "m"
	notes := []string{"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"}
	switch tonic {
	case "F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb":
		notes = []string{"C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"}
	}
	var index int
	for index1 := range notes {
		if strings.EqualFold(notes[index1], tonic) {
			index = index1
			break
		}
	}
	scale := make([]string, len(intervals))
	for index2, value := range intervals {
		scale[index2] = notes[(index)%len(notes)]
		index += steps[value]
	}
	return scale
}

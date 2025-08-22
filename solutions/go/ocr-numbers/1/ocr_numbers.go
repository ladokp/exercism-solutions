package ocr

import "strings"

var digit = map[string]byte{
	" _ | ||_|   ": '0',
	"     |  |   ": '1',
	" _  _||_    ": '2',
	" _  _| _|   ": '3',
	"   |_|  |   ": '4',
	" _ |_  _|   ": '5',
	" _ |_ |_|   ": '6',
	" _   |  |   ": '7',
	" _ |_||_|   ": '8',
	" _ |_| _|   ": '9',
}

func recognizeDigit(lines []string, start int) byte {
	if len(lines) < 4 {
		return '?'
	}
	need := start + 3
	for _, line := range lines {
		if len(line) < need {
			return '?'
		}
	}
	if d, ok := digit[lines[0][start:need]+
		lines[1][start:need]+
		lines[2][start:need]+
		lines[3][start:need]]; ok {
		return d
	}
	return '?'
}

func Recognize(s string) []string {
	lines := strings.Split(s, "\n")
	if lines[0] == "" {
		lines = lines[1:]
	}
	r := make([]string, (len(lines)+3)/4)
	for index := range r {
		max_ := 0
		for lineIndex := 0; lineIndex < 4 && lineIndex < len(lines); lineIndex++ {
			if n := len(lines[lineIndex]); n > max_ {
				max_ = n
			}
		}
		b := make([]byte, (max_+2)/3)
		for j := range b {
			b[j] = recognizeDigit(lines, j*3)
		}
		r[index] = string(b)
		lines = lines[4:]
	}
	return r
}

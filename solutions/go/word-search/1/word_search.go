package wordsearch

import "fmt"

type dir struct {
	cx, cy int
}

var dirs = []dir{
	{0, -1},
	{1, -1},
	{1, 0},
	{1, 1},
	{0, 1},
	{-1, 1},
	{-1, 0},
	{-1, -1},
}

var zeroSpan = [2][2]int{{0, 0}, {0, 0}}

func find(word string, puzzle []string) ([2][2]int, error) {
	for x := 0; x < len(puzzle[0]); x++ {
		for y := 0; y < len(puzzle); y++ {
			for _, dir := range dirs {
				if span, ok := try(word, puzzle, x, y, dir); ok {
					return span, nil
				}
			}
		}
	}
	return zeroSpan, fmt.Errorf("didn't find %q", word)
}

func try(word string, puzzle []string, startX, startY int, dir dir) ([2][2]int, bool) {
	x, y := startX, startY
	for _, character := range word {
		if lc, ok := lookup(x, y, puzzle); !ok || lc != character {
			return zeroSpan, false
		}
		x += dir.cx
		y += dir.cy
	}
	return [2][2]int{{startX, startY}, {x - dir.cx, y - dir.cy}}, true
}

func lookup(x, y int, puzzle []string) (rune, bool) {
	if x >= 0 && x < len(puzzle[0]) && y >= 0 && y < len(puzzle) {
		return rune(puzzle[y][x]), true
	}
	return 0, false
}

func Solve(words, puzzle []string) (map[string][2][2]int, error) {
	positions := make(map[string][2][2]int, len(words))
	for _, word := range words {
		span, err := find(word, puzzle)
		if err != nil {
			return nil, err
		}
		positions[word] = span
	}
	return positions, nil
}

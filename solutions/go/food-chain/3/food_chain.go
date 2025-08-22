package foodchain

import (
	"strings"
)

type Animal struct {
	name string
	line string
	food string
}

var foodChain = map[int]Animal{
	1: {name: "fly", line: "I don't know why she swallowed the fly. Perhaps she'll die.", food: ""},
	2: {name: "spider", line: "It wriggled and jiggled and tickled inside her.", food: "fly"},
	3: {name: "bird", line: "How absurd to swallow a bird!", food: "spider that wriggled and jiggled and tickled inside her"},
	4: {name: "cat", line: "Imagine that, to swallow a cat!", food: "bird"},
	5: {name: "dog", line: "What a hog, to swallow a dog!", food: "cat"},
	6: {name: "goat", line: "Just opened her throat and swallowed a goat!", food: "dog"},
	7: {name: "cow", line: "I don't know how she swallowed a cow!", food: "goat"},
	8: {name: "horse", line: "She's dead, of course!", food: ""},
}

func Verse(last int) string {
	var sb strings.Builder
	sb.WriteString("I know an old lady who swallowed a ")
	sb.WriteString(foodChain[last].name)
	sb.WriteString(".\n")
	sb.WriteString(foodChain[last].line)
	if foodChain[last].food == "" {
		return sb.String()
	}
	for i := last; i > 1; i-- {
		sb.WriteString("\nShe swallowed the ")
		sb.WriteString(foodChain[i].name)
		sb.WriteString(" to catch the ")
		sb.WriteString(foodChain[i].food)
		sb.WriteString(".")
	}
	sb.WriteString("\n")
	sb.WriteString(foodChain[1].line)
	return sb.String()
}

func Verses(first, last int) string {
	var sb strings.Builder
	for index := first; index <= last; index++ {
		sb.WriteString(Verse(index))
		if index != last {
			sb.WriteString("\n\n")
		}
	}
	return sb.String()
}

func Song() string {
	return Verses(1, len(foodChain))
}

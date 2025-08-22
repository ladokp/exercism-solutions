package foodchain

import (
	"strings"
)

type Animal struct {
	name  string
	bLine string
	toEat string
}

var foodChain = map[int]Animal{
	1: {name: "fly", bLine: "I don't know why she swallowed the fly. Perhaps she'll die.", toEat: ""},
	2: {name: "spider", bLine: "It wriggled and jiggled and tickled inside her.", toEat: "fly"},
	3: {name: "bird", bLine: "How absurd to swallow a bird!", toEat: "spider that wriggled and jiggled and tickled inside her"},
	4: {name: "cat", bLine: "Imagine that, to swallow a cat!", toEat: "bird"},
	5: {name: "dog", bLine: "What a hog, to swallow a dog!", toEat: "cat"},
	6: {name: "goat", bLine: "Just opened her throat and swallowed a goat!", toEat: "dog"},
	7: {name: "cow", bLine: "I don't know how she swallowed a cow!", toEat: "goat"},
	8: {name: "horse", bLine: "She's dead, of course!", toEat: ""},
}

func Verse(last int) string {
	var sb strings.Builder
	sb.WriteString("I know an old lady who swallowed a ")
	sb.WriteString(foodChain[last].name)
	sb.WriteString(".\n")
	sb.WriteString(foodChain[last].bLine)
	if foodChain[last].toEat == "" {
		return sb.String()
	}
	for i := last; i > 1; i-- {
		sb.WriteString("\nShe swallowed the ")
		sb.WriteString(foodChain[i].name)
		sb.WriteString(" to catch the ")
		sb.WriteString(foodChain[i].toEat)
		sb.WriteString(".")
	}
	sb.WriteString("\n")
	sb.WriteString(foodChain[1].bLine)
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

// Package proverb provides a method to build a proverb from a given List
package proverb

import "fmt"

// Proverb is created based on a given list of words
func Proverb(rhyme []string) (text []string) {
	if len(rhyme) == 0 {
		return
	}
	for index, word := range rhyme {
		if index == len(rhyme)-1 {
			break
		}
		text = append(text, fmt.Sprintf("For want of a %s the %s was lost.", word, rhyme[index+1]))
	}
	text = append(text, fmt.Sprintf("And all for the want of a %s.", rhyme[0]))
	return
}

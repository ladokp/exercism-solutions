package piglatin

import (
	"regexp"
	"strings"
)

const (
	vowelPattern       = `^([aeiou]|xr|yt)`
	consonantPattern   = `^([^aeiou]+)`
	consonantQuPattern = `^([^aeiouy]*)qu`
	consonantYPattern  = `^([bcdfghjklmnpqrstvwz]+)y`
)

// Sentence takes a plain text sentence and returns its Pig Latin equivalent.
func Sentence(plainText string) string {
	var pigLatin []string
	var consonantSound string
	for _, word := range strings.Fields(plainText) {
		switch {
		case regexp.MustCompile(vowelPattern).MatchString(word):
			pigLatin = append(pigLatin, word+"ay")
		case regexp.MustCompile(consonantQuPattern).MatchString(word):
			consonantSound = getConsonantSound(word, consonantQuPattern) + "qu"
			pigLatin = append(pigLatin, word[len(consonantSound):]+consonantSound+"ay")
		case regexp.MustCompile(consonantYPattern).MatchString(word):
			consonantSound = getConsonantSound(word, consonantYPattern)
			pigLatin = append(pigLatin, word[len(consonantSound):]+consonantSound+"ay")
		case regexp.MustCompile(consonantPattern).MatchString(word):
			consonantSound = getConsonantSound(word, consonantPattern)
			pigLatin = append(pigLatin, word[len(consonantSound):]+consonantSound+"ay")
		}
	}
	return strings.Join(pigLatin, " ")
}

func getConsonantSound(s, pattern string) string {
	return regexp.MustCompile(pattern).FindStringSubmatch(s)[1]
}

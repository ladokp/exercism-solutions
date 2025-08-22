package encode

import (
	"fmt"
	"strings"
)

func RunLengthEncode(input string) string {
	var builder strings.Builder
	number := len(input)
	for index := 0; index < number; index++ {
		currentCount := 1
		for index < number-1 && input[index] == input[index+1] {
			currentCount++
			index++
		}
		if currentCount != 1 {
			builder.WriteString(compress(input[index], currentCount))
		} else {
			builder.WriteString(string(input[index]))
		}
	}
	return builder.String()
}
func RunLengthDecode(s string) (result string) {
	var builder strings.Builder
	count := 0
	for index := 0; index < len(s); {
		if s[index] >= '0' && s[index] <= '9' {
			count = (count * 10) + (int(s[index]) - '0')
		} else {
			if count == 0 {
				builder.WriteString(string(s[index]))
			} else {
				builder.WriteString(repeat(s[index], count))
				count = 0
			}
		}
		index++
	}
	return builder.String()
}
func compress(character byte, count int) string {
	return fmt.Sprintf("%d%c", count, character)
}
func repeat(character byte, count int) string {
	var builder strings.Builder
	for index := 0; index < count; index++ {
		builder.WriteString(string(character))
	}
	return builder.String()
}

package brackets

var matchingBrackets = map[rune]rune{
	')': '(',
	'}': '{',
	']': '[',
}

// Bracket verifies that any and all pairs of a given string containing brackets `[]`, braces `{}`,
// parentheses `()`, or any combination thereof, are matched correctly.
func Bracket(input string) bool {
	brackets := make([]rune, 0)
	for _, bracket := range input {
		switch bracket {
		case '(', '{', '[':
			brackets = append(brackets, bracket)
		case ')', '}', ']':
			if len(brackets) == 0 || brackets[len(brackets)-1] != matchingBrackets[bracket] {
				return false
			}
			brackets = brackets[:len(brackets)-1]
		}
	}
	return len(brackets) == 0
}

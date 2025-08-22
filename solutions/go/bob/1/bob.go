package bob

import "strings"

func isQuestion(sentence string) bool {
	return strings.HasSuffix(sentence, "?")
}
func isYelling(sentence string) bool {
	return strings.ToUpper(sentence) == sentence && strings.ToLower(sentence) != sentence
}
func isQuestionYelling(sentence string) bool {
	return isQuestion(sentence) && isYelling(sentence)
}
func isEmpty(sentence string) bool {
	return sentence == ""
}
func Hey(remark string) string {
	remark = strings.TrimSpace(remark)
	switch {
	case isQuestionYelling(remark):
		return "Calm down, I know what I'm doing!"
	case isQuestion(remark):
		return "Sure."
	case isYelling(remark):
		return "Whoa, chill out!"
	case isEmpty(remark):
		return "Fine. Be that way!"
	default:
		return "Whatever."
	}
}

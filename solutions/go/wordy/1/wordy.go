package wordy

import (
	"regexp"
	"strconv"
)

func Answer(question string) (int, bool) {
	if match, _ := regexp.MatchString(`What is -?\d+(?: (?:plus|minus|divided by|multiplied by) -?\d+)*\?`, question); !match {
		return 0, false
	}

	operators := regexp.MustCompile(`(plus|minus|divided|multiplied)`).FindAllString(question, -1)
	numbers := regexp.MustCompile(`-?\d+`).FindAllString(question, -1)
	if len(operators) != len(numbers)-1 {
		return 0, false
	}

	sum, _ := strconv.Atoi(numbers[0])
	for index, operator := range operators {
		number, _ := strconv.Atoi(numbers[index+1])
		switch operator {
		case "plus":
			sum += number
		case "minus":
			sum -= number
		case "divided":
			sum /= number
		case "multiplied":
			sum *= number
		}
	}
	return sum, true
}

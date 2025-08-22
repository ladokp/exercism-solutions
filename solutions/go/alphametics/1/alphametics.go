package alphametics

import (
	"errors"
	"unicode"
)

const (
	NonDigit    byte = 10
	NonLetter        = 26
	EmptyLetter      = 11
)

type Alphametics struct {
	puzzle    string
	equation  [][]byte
	variables [26]byte
	digits    [10]byte
	letters   []byte
	first     [26]bool
}

func (alpha *Alphametics) LetterExists(letter byte) bool {
	return alpha.variables[letter] != EmptyLetter
}

func (alpha *Alphametics) LetterValue(letter byte) int {
	return int(alpha.variables[letter])
}

func (alpha *Alphametics) IsSet(letter byte) bool {
	return alpha.variables[letter] != NonDigit
}

func (alpha *Alphametics) SetLetterValue(letter byte, val byte) {
	if alpha.variables[letter] < NonDigit {
		alpha.digits[alpha.variables[letter]] = NonLetter
	}
	if val < NonDigit {
		alpha.digits[val] = letter
	}
	alpha.variables[letter] = val
}

func (alpha *Alphametics) IsFirstDigit(letter byte) bool {
	return alpha.first[letter]
}

func (alpha *Alphametics) FirstDigit(letter byte) {
	alpha.first[letter] = true
}

func (alpha *Alphametics) IsUnique(val int) bool {
	return alpha.digits[val] == NonLetter
}

func (alpha *Alphametics) Number(n int) (returnValue int) {
	arr := alpha.equation[n]
	for i := 0; i < len(arr); i++ {
		returnValue = returnValue*10 + alpha.LetterValue(arr[i])
	}
	return returnValue
}

func New(puzzle string) *Alphametics {
	var digits [10]byte
	var variables [26]byte
	for i := 0; i < len(variables); i++ {
		variables[i] = EmptyLetter
	}
	for i := 0; i < len(digits); i++ {
		digits[i] = NonLetter
	}
	return &Alphametics{
		puzzle:    puzzle,
		digits:    digits,
		variables: variables}
}

func (alpha *Alphametics) Parse() error {
	maximum1, maximum2 := 0, 0
	var equation []byte
	for _, ch := range alpha.puzzle {
		if unicode.IsLetter(ch) {
			b := byte(ch - 'A')
			if equation == nil {
				alpha.FirstDigit(b)
			}
			if !alpha.LetterExists(b) {
				alpha.SetLetterValue(b, NonDigit)
				alpha.letters = append(alpha.letters, b)
			}
			equation = append(equation, b)
		} else if equation != nil {
			if len(equation) > maximum1 {
				maximum1 = len(equation)
				maximum2 = len(alpha.equation)
			}
			alpha.equation = append(alpha.equation, equation)
			equation = nil
		}
	}
	if equation != nil {
		alpha.equation = append(alpha.equation, equation)
		equation = nil
	}
	if len(alpha.letters) > 10 || len(alpha.letters) < 1 {
		return errors.New("more than 10 letters")
	}
	if len(alpha.equation) < 2 {
		return errors.New("at least two parts should be in equation")
	}
	if maximum2 == len(alpha.equation)-1 {
		alpha.variables[alpha.equation[maximum2][0]] = 1
	}
	return nil
}

func (alpha *Alphametics) Solution() (solution map[string]int) {
	solution = make(map[string]int)
	for _, b := range alpha.letters {
		r := rune(b + 'A')
		solution[string(r)] = alpha.LetterValue(b)
	}
	return
}

func (alpha *Alphametics) Solve(number int) bool {
	if number == len(alpha.letters) {
		return alpha.Evaluate()
	}
	letter := alpha.letters[number]
	if alpha.IsSet(letter) {
		return alpha.Solve(number + 1)
	}
	for index := 0; index <= 9; index++ {
		if (index == 0 && alpha.IsFirstDigit(letter)) || !alpha.IsUnique(index) {
			continue
		}
		alpha.SetLetterValue(letter, byte(index))
		if alpha.Solve(number + 1) {
			return true
		}
	}
	alpha.SetLetterValue(letter, NonDigit)
	return false
}

func (alpha *Alphametics) Evaluate() bool {
	value, index := 0, 0
	for ; index < len(alpha.equation)-1; index++ {
		value += alpha.Number(index)
	}
	return alpha.Number(index) == value
}

func Solve(puzzle string) (map[string]int, error) {
	alpha := New(puzzle)
	if err := alpha.Parse(); err != nil {
		return nil, err
	}
	if alpha.Solve(0) {
		return alpha.Solution(), nil
	}
	return nil, errors.New("not unique solution")
}

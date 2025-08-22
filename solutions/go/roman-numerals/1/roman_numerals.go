package romannumerals

import "errors"

type Pair struct {
	decimal int
	roman   string
}

var valueSymbols = []Pair{
	{1000, "M"},
	{900, "CM"},
	{500, "D"},
	{400, "CD"},
	{100, "C"},
	{90, "XC"},
	{50, "L"},
	{40, "XL"},
	{10, "X"},
	{9, "IX"},
	{5, "V"},
	{4, "IV"},
	{1, "I"},
}

func ToRomanNumeral(input int) (string, error) {
	romanNumbers := ""
	if input < 1 || input > 3999 {
		return romanNumbers, errors.New("invalid number")
	}
	remainder := input
	for _, pair := range valueSymbols {
		for {
			if remainder < pair.decimal {
				break
			}
			romanNumbers += pair.roman
			remainder -= pair.decimal
		}
	}
	return romanNumbers, nil
}

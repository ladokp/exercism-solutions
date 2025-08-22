package raindrops

import "strconv"

func Convert(number int) string {
	stringToReturn := ""
	if number%3 == 0 {
		stringToReturn += "Pling"
	}
	if number%5 == 0 {
		stringToReturn += "Plang"
	}
	if number%7 == 0 {
		stringToReturn += "Plong"
	}
	if stringToReturn == "" {
		return strconv.Itoa(number)
	}
	return stringToReturn
}

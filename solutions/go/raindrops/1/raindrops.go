package raindrops

import "fmt"

func Convert(number int) string {
    stringToReturn := ""
	if number % 3 == 0 {
        stringToReturn += "Pling"
    }
	if number % 5 == 0 {
        stringToReturn +=  "Plang"
    }
	if number % 7 == 0 {
        stringToReturn +=  "Plong"
    }
	if stringToReturn == "" {
		return fmt.Sprintf("%d", number)
    }
	return stringToReturn
}

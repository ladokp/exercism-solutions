package resistorcolortrio

import ( "fmt" )
import ( "math" )

// ColorCode returns the resistance value of the given color.
func ColorCode(color string) int {
    colors := []string{
		"black",
		"brown",
		"red",
		"orange",
		"yellow",
		"green",
		"blue",
		"violet",
		"grey",
		"white",
	}
    for index := range colors {
        if colors[index] == color {
            return index
        }
    }
    return -1
}

// Label describes the resistance value given the colors of a resistor.
// The label is a string with a resistance value with an unit appended
// (e.g. "33 ohms", "470 kiloohms").
func Label(colors []string) string {
    resistanceValue := (10 * ColorCode(colors[0]) + ColorCode(colors[1])) * int(math.Pow(10, float64(ColorCode(colors[2]))))
    unit := "ohms"
    if resistanceValue >= 1000000000 {
        resistanceValue /= 1000000000
        unit = "gigaohms"
    } else if resistanceValue >= 1000000 {
        resistanceValue /= 1000000
        unit = "megaohms"
    } else if resistanceValue >= 1000 {
        resistanceValue /= 1000
        unit = "kiloohms"
    } 
    return fmt.Sprintf("%d %s", resistanceValue, unit)
}

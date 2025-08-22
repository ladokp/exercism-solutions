package resistorcolorduo

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

// Value should return the resistance value of a resistor with a given colors.
func Value(colors []string) int {
    return 10 * ColorCode(colors[0]) + ColorCode(colors[1])
}

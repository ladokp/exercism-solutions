package allergies

var allergenMap = map[string]uint{
	"eggs":         1,
	"peanuts":      2,
	"shellfish":    4,
	"strawberries": 8,
	"tomatoes":     16,
	"chocolate":    32,
	"pollen":       64,
	"cats":         128,
}

func Allergies(allergies uint) (result []string) {
	for allergen := range allergenMap {
		if AllergicTo(allergies, allergen) {
			result = append(result, allergen)
		}
	}
	return
}

func AllergicTo(allergies uint, allergen string) (result bool) {
	score, found := allergenMap[allergen]
	result = found && allergies&score != 0
	return
}

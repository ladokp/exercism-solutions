package allergies

var allergenMap = map[string]uint{
	"eggs":         1 << 0,
	"peanuts":      1 << 1,
	"shellfish":    1 << 2,
	"strawberries": 1 << 3,
	"tomatoes":     1 << 4,
	"chocolate":    1 << 5,
	"pollen":       1 << 6,
	"cats":         1 << 7,
}

func Allergies(allergies uint) (result []string) {
	for allergen := range allergenMap {
		if AllergicTo(allergies, allergen) {
			result = append(result, allergen)
		}
	}
	return
}

func AllergicTo(allergies uint, allergen string) bool {
	score, found := allergenMap[allergen]
	return found && allergies&score != 0
}

package allergies

var allergenMap = map[string]int{
	"eggs":         1,
	"peanuts":      2,
	"shellfish":    3,
	"strawberries": 4,
	"tomatoes":     5,
	"chocolate":    6,
	"pollen":       7,
	"cats":         8,
}

func Allergies(allergies uint) (result []string) {
	for allergen, _ := range allergenMap {
		if AllergicTo(allergies, allergen) {
			result = append(result, allergen)
		}
	}
	return
}

func AllergicTo(allergies uint, allergen string) bool {
	score, found := allergenMap[allergen]
	if found != true {
		return false
	}
	return allergies&(1<<(score-1)) != 0
}

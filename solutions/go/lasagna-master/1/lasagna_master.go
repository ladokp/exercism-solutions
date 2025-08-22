package lasagna

// PreparationTime returns the average preparation time for given layers
func PreparationTime(layers []string, averagePreparationTimePerLayer int) int {
    if averagePreparationTimePerLayer == 0 {
		averagePreparationTimePerLayer = 2   
    }
    return len(layers) * averagePreparationTimePerLayer
}

// Quantities returns the needed noodles and sauce amounts
func Quantities(layers []string) (int, float64) {
    var neededNoodles int = 0
    var neededSauce float64 = 0.0
    for i := 0; i < len(layers); i++ {
        if layers[i] == "noodles" {
            neededNoodles += 50
        }
        if layers[i] == "sauce" {
            neededSauce += 0.2
        }
    }
    return neededNoodles, neededSauce
}

// AddSecretIngredient add the secret ingriedient to the list
func AddSecretIngredient(friendsList []string, myList []string) []string {
    newList := make([]string, len(myList)+1)
    for i := 0; i < len(myList); i++ {
		newList[i] = myList[i]
    }
	newList[len(newList)-1] = friendsList[len(friendsList)-1]
    return newList
}

// ScaleRecipe scales a recipe to the given portions
func ScaleRecipe(amounts []float64, portions int) []float64 {
    var scaleFactor = float64(portions) / float64(2)
    scaledAmounts := make([]float64, len(amounts))
    for i := 0; i < len(scaledAmounts); i++ {
		scaledAmounts[i] = amounts[i] * float64(scaleFactor)
    }
	return scaledAmounts
}

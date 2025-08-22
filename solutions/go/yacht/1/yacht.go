package yacht

func sumOccurrences(dice []int, eyes int) (result int) {
	for _, element := range dice {
		if element == eyes {
			result += eyes
		}
	}
	return
}

func countFourOfAKind(dice []int) int {
	for i := 1; i <= 6; i++ {
		count := 0
		for _, die := range dice {
			if die == i {
				count++
			}
		}
		if count >= 4 {
			return 4 * i
		}
	}
	return 0
}

func countYacht(dice []int) int {
	for i := 1; i <= 6; i++ {
		count := 0
		for _, die := range dice {
			if die == i {
				count++
			}
		}
		if count == 5 {
			return 50
		}
	}
	return 0
}

func countFullHouse(dice []int) int {
	hasTwoDice := false
	hasThreeDice := false
	for i := 1; i <= 6; i++ {
		count := 0
		for _, die := range dice {
			if die == i {
				count++
			}
		}
		if count == 2 {
			hasTwoDice = true
			continue
		}
		if count == 3 {
			hasThreeDice = true
			continue
		}
	}
	if hasTwoDice && hasThreeDice {
		return sum(dice, len(dice))
	}
	return 0
}

func countLittleStraight(dice []int) int {
	for i := 1; i < 6; i++ {
		found := false
		for _, element := range dice {
			if element == i {
				found = true
				break
			}
		}
		if !found {
			return 0
		}
	}
	return 30
}

func countBigStraight(dice []int) int {
	for i := 2; i <= 6; i++ {
		found := false
		for _, element := range dice {
			if element == i {
				found = true
				break
			}
		}
		if !found {
			return 0
		}
	}
	return 30
}

func sum(slice []int, n int) int {
	if n <= 0 {
		return 0
	}
	return sum(slice, n-1) + slice[n-1]
}

func Score(dice []int, category string) int {
	switch category {
	case "ones":
		return sumOccurrences(dice, 1)
	case "twos":
		return sumOccurrences(dice, 2)
	case "threes":
		return sumOccurrences(dice, 3)
	case "fours":
		return sumOccurrences(dice, 4)
	case "fives":
		return sumOccurrences(dice, 5)
	case "sixes":
		return sumOccurrences(dice, 6)
	case "choice":
		return sum(dice, len(dice))
	case "four of a kind":
		return countFourOfAKind(dice)
	case "full house":
		return countFullHouse(dice)
	case "little straight":
		return countLittleStraight(dice)
	case "big straight":
		return countBigStraight(dice)
	case "yacht":
		return countYacht(dice)
	}
	return 0
}

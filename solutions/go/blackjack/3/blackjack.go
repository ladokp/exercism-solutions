package blackjack

var CARDS = map[string]int{
	"two":   2,
	"three": 3,
	"four":  4,
	"five":  5,
	"six":   6,
	"seven": 7,
	"eight": 8,
	"nine":  9,
	"ten":   10,
	"jack":  10,
	"queen": 10,
	"king":  10,
	"ace":   11,
}

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) int {
	if points, found := CARDS[card]; found {
		return points
	} else {
		return 0
	}
}

// IsBlackjack returns true if the player has a blackjack, false otherwise.
func IsBlackjack(card1, card2 string) bool {
	return ParseCard(card1)+ParseCard(card2) == 21
}

// LargeHand implements the decision tree for hand scores larger than 20 points.
func LargeHand(isBlackjack bool, dealerScore int) string {
	if isBlackjack && dealerScore < 10 {
		return "W"
	}
	if !isBlackjack {
		return "P"
	}
	return "S"
}

// SmallHand implements the decision tree for hand scores with less than 21 points.
func SmallHand(handScore, dealerScore int) string {
	if handScore >= 17 {
		return "S"
	} else if handScore <= 11 {
		return "H"
	} else if dealerScore < 7 {
		return "S"
	} else {
		return "H"
	}
}

func FirstTurn(card1, card2, dealerCard string) string {
	ownScore := ParseCard(card1) + ParseCard(card2)
	dealerScore := ParseCard(dealerCard)
	if ownScore < 21 {
		return SmallHand(ownScore, dealerScore)
	}
	return LargeHand(IsBlackjack(card1, card2), dealerScore)
}

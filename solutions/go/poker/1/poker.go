package poker

import (
	"fmt"
	"sort"
	"strings"
	"unicode/utf8"
)

const (
	Jack  = 11
	Queen = 12
	King  = 13
	Ace   = 14
)

type kind int

const (
	highCard kind = iota
	onePair
	twoPair
	threeOfAKind
	straight
	flush
	fullHouse
	fourOfAKind
	straightFlush
)

type ordering int

const (
	lessThan    ordering = -1
	equalTo     ordering = 0
	greaterThan ordering = 1
)

type card struct {
	rank int
	suit rune
}

type rankCount struct {
	rank  int
	count int
}

type rankCountSlice []rankCount

func (slice rankCountSlice) Len() int      { return len(slice) }
func (slice rankCountSlice) Swap(i, j int) { slice[i], slice[j] = slice[j], slice[i] }
func (slice rankCountSlice) Less(i, j int) bool {
	if slice[i].count != slice[j].count {
		return slice[i].count < slice[j].count
	}
	return slice[i].rank < slice[j].rank
}

type handValue struct {
	kind           kind
	discriminators []int
}

func (hv handValue) Compare(other handValue) ordering {
	if hv.kind == other.kind {
		if len(hv.discriminators) != len(other.discriminators) {
			// Shouldn't happen, number of discriminators for a kind is fixed
			panic("Hands of same kind with different number of discriminators")
		}
		for i := 0; i < len(hv.discriminators); i++ {
			if hv.discriminators[i] != other.discriminators[i] {
				if hv.discriminators[i] < other.discriminators[i] {
					return lessThan
				}
				return greaterThan
			}
		}
		return equalTo
	} else if hv.kind < other.kind {
		return lessThan
	}
	return greaterThan
}

func BestHand(hands []string) ([]string, error) {
	values := make([]handValue, len(hands))
	for i, hand := range hands {
		cards, err := parseHand(hand)
		if err != nil {
			return nil, err
		}
		counts := countRanks(cards)
		values[i] = evalHand(counts, cards)
	}

	idxes := selectBest(values)
	best := make([]string, len(idxes))
	for i, idx := range idxes {
		best[i] = hands[idx]
	}
	return best, nil
}

func selectBest(values []handValue) []int {
	var best []int
	for i, hv := range values {
		if len(best) == 0 {
			best = []int{i}
		} else {
			switch values[best[0]].Compare(hv) {
			case lessThan:
				best = []int{i}
			case equalTo:
				best = append(best, i)
			}
		}
	}
	return best
}

func evalHand(counts []rankCount, cards []card) handValue {
	isFlush := isFlush(cards)
	isStraight := isStraight(counts)
	var kind kind
	switch {
	case isStraight && isFlush:
		kind = straightFlush
	case counts[0].count == 4:
		kind = fourOfAKind
	case counts[0].count == 3 && counts[1].count == 2:
		kind = fullHouse
	case isFlush:
		kind = flush
	case isStraight:
		kind = straight
	case counts[0].count == 3:
		kind = threeOfAKind
	case counts[0].count == 2 && counts[1].count == 2:
		kind = twoPair
	case counts[0].count == 2:
		kind = onePair
	default:
		kind = highCard
	}
	discriminators := make([]int, 0, len(cards))
	switch {
	case kind == flush:
		for _, card := range cards {
			discriminators = append(discriminators, card.rank)
		}
		sort.Sort(sort.Reverse(sort.IntSlice(discriminators)))
	case (kind == straight || kind == straightFlush) &&
		counts[0].rank == Ace && counts[1].rank == 5:
		for i := 1; i < len(cards); i++ {
			discriminators = append(discriminators, counts[i].rank)
		}
		discriminators = append(discriminators, 1)
	default:
		for _, rc := range counts {
			discriminators = append(discriminators, rc.rank)
		}
	}
	return handValue{kind, discriminators}
}

func isFlush(cards []card) bool {
	first := cards[0].suit
	for i := 1; i < len(cards); i++ {
		if cards[i].suit != first {
			return false
		}
	}
	return true
}

func isStraight(counts []rankCount) bool {
	if len(counts) != 5 {
		return false
	}
	first := counts[0].rank
	if first == Ace {
		for i := 1; i < len(counts); i++ { //nolint // false positive
			if counts[i].rank != 6-i {
				break
			}
			return true
		}
	}
	for i := 1; i < len(counts); i++ {
		if counts[i].rank != first-i {
			return false
		}
	}
	return true
}

func countRanks(cards []card) []rankCount {
	var counts []rankCount
loop:
	for _, card := range cards {
		for i, rc := range counts {
			if rc.rank == card.rank {
				counts[i] = rankCount{rc.rank, rc.count + 1}
				continue loop
			}
		}
		counts = append(counts, rankCount{card.rank, 1})
	}
	sort.Sort(sort.Reverse(rankCountSlice(counts)))
	return counts
}

func parseHand(s string) ([]card, error) {
	parts := strings.Split(strings.TrimSpace(s), " ")
	if len(parts) != 5 {
		return nil, fmt.Errorf("invalid number of hand parts: %d", len(parts))
	}
	cards := make([]card, 5)
	for i, part := range parts {
		card, err := parseCard(part)
		if err != nil {
			return nil, err
		}
		cards[i] = card
	}
	return cards, nil
}

func parseCard(s string) (card, error) {
	if len(s) < 1 {
		return card{}, fmt.Errorf("too short card string: %q", s)
	}
	var rank int
	suitStart := 1
	if s[0] >= '2' && s[0] <= '9' {
		rank = int(s[0]) - '0'
	} else {
		switch s[0] {
		case '1':
			if len(s) < 2 || s[1] != '0' {
				return card{}, fmt.Errorf("invalid rank in card string: %q", s)
			}
			rank = 10
			suitStart = 2
		case 'J':
			rank = Jack
		case 'Q':
			rank = Queen
		case 'K':
			rank = King
		case 'A':
			rank = Ace
		default:
			return card{}, fmt.Errorf("invalid rank in card string: %q", s)
		}
	}
	suit, size := utf8.DecodeRuneInString(s[suitStart:])
	if suit == utf8.RuneError {
		return card{}, fmt.Errorf("invalid UTF-8 character in card string: %q", s)
	} else if !(suit == '♡' || suit == '♢' || suit == '♤' || suit == '♧') {
		return card{}, fmt.Errorf("invalid suit: %c", suit)
	}
	if suitStart+size != len(s) {
		return card{}, fmt.Errorf("extraneous data after suit in card string: %q", s)
	}
	return card{rank, suit}, nil
}

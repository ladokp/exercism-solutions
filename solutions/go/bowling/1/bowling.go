package bowling

import "fmt"

const (
	pinsPerFrame  = 10
	framesPerGame = 10
	rollsPerFrame = 2
)

type Game struct {
	framesCompleted int
	score           int
	pinsUp          int
	rolls           int
	bonus1          int
	bonus2          int
}

func NewGame() *Game {
	return &Game{pinsUp: pinsPerFrame}
}

func (game *Game) isComplete() bool {
	return game.framesCompleted >= framesPerGame && game.bonus1 == 0 && game.bonus2 == 0
}

func (game *Game) Roll(pins int) error {
	if pins < 0 {
		return fmt.Errorf("can't roll negative pins %d", pins)
	}
	if pins > game.pinsUp {
		return fmt.Errorf("there are only %d pins up, can't roll %d", game.pinsUp, pins)
	}
	if game.isComplete() {
		return fmt.Errorf("game complete, can't roll %d", pins)
	}

	game.rolls++
	game.pinsUp -= pins
	multiplier := 1
	isFill := game.framesCompleted >= framesPerGame
	if isFill {
		multiplier = 0
	}
	multiplier += game.bonus1 + game.bonus2
	game.score += pins * multiplier
	game.bonus1 = game.bonus2
	game.bonus2 = 0

	if game.pinsUp == 0 || game.rolls == rollsPerFrame {
		if game.pinsUp == 0 && !isFill {
			if game.rolls == rollsPerFrame {
				game.bonus1++
			} else {
				game.bonus2++
			}
		}
		game.rolls = 0
		game.pinsUp = pinsPerFrame
		game.framesCompleted++
	}
	return nil
}

func (game *Game) Score() (int, error) {
	if !game.isComplete() {
		return 0, fmt.Errorf("game incomplete: On frame %d", game.framesCompleted+1)
	}
	return game.score, nil
}

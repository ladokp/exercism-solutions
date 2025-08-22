package dndcharacter

import (
    "math"
    "math/rand"
)

type Character struct {
	Strength     int
	Dexterity    int
	Constitution int
	Intelligence int
	Wisdom       int
	Charisma     int
	Hitpoints    int
}

// Modifier calculates the ability modifier for a given ability score
func Modifier(score int) int {
	return int(math.Floor((float64(score) - 10) / 2))
}

// Ability uses randomness to generate the score for an ability
func Ability() int {
    smallestValue, randomSum := 6, 0
    for i := 1; i <= 4; i++ {
        randomValue := rand.Intn(6) + 1
        randomSum += randomValue
        if randomValue < smallestValue {
            smallestValue = randomValue
        }
    }
	return randomSum - smallestValue
}

// GenerateCharacter creates a new Character with random scores for abilities
func GenerateCharacter() Character {
	var character Character
    character.Strength = Ability()
	character.Dexterity = Ability()
	character.Constitution = Ability()
	character.Intelligence = Ability()
	character.Wisdom = Ability()    
	character.Charisma = Ability()
	character.Hitpoints = 10 + Modifier(character.Constitution)
    return character
}

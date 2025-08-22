package twobucket

import (
	"errors"
	"math"
)

func gcd(first, second int) int {
	if second != 0 {
		return gcd(second, first%second)
	} else {
		return first
	}
}

func Solve(firstBucket, secondBucket, goal int, starter string) (string, int, int, error) {
	if firstBucket < 1 || secondBucket < 1 || (goal > firstBucket && goal > secondBucket) || goal%gcd(firstBucket, secondBucket) != 0 || (starter != "one" && starter != "two") || goal == 0 {
		return "", 0, 0, errors.New("")
	}

	start := 0
	if starter == "two" {
		start = 1
	}
	moves := 1
	buck := []map[string]int{{"max": firstBucket, "filled": 0}, {"max": secondBucket, "filled": 0}}
	buck[start]["filled"] = buck[start]["max"]

	for buck[0]["filled"] != goal && buck[1]["filled"] != goal {
		if (moves == 1) && buck[^start+2]["max"] == goal {
			buck[^start+2]["filled"] = goal
		} else if buck[start]["filled"] == 0 {
			buck[start]["filled"] = buck[start]["max"]
		} else if buck[^start+2]["filled"] == 0 || buck[start]["filled"] == buck[start]["max"] {
			transfer := int(math.Min(float64(buck[start]["filled"]), float64(buck[^start+2]["max"]-buck[^start+2]["filled"])))

			buck[start]["filled"] -= transfer
			buck[^start+2]["filled"] += transfer
		} else if buck[^start+2]["filled"] == buck[^start+2]["max"] {
			buck[^start+2]["filled"] = 0
		}
		moves++
	}

	goalBucket := "two"
	index := 0
	if buck[0]["filled"] == goal {
		index = 1
		goalBucket = "one"
	}
	return goalBucket, moves, buck[index]["filled"], nil
}

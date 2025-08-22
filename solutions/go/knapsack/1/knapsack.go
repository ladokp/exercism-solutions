package knapsack

// Item represents an item with a weight and a value.
type Item struct {
	Weight, Value int
}

// Knapsack computes the maximum value that can be obtained
// by selecting items without exceeding the maximum weight capacity.
// The function uses dynamic programming to solve the 0/1 knapsack problem.
func Knapsack(maximumWeight int, items []Item) int {
	amountOfItems := len(items)
	
	// Create a 2D slice to store the maximum values for subproblems.
	// knapsack[i][w] holds the maximum value that can be obtained
	// with the first i items and a maximum weight limit of w.
	knapsack := make([][]int, amountOfItems+1)

	for i := range knapsack {
		knapsack[i] = make([]int, maximumWeight+1)
	}

	// Fill the knapsack table using dynamic programming.
	for i := 1; i <= amountOfItems; i++ {
		for w := 0; w <= maximumWeight; w++ {
			// If the current item's weight is more than the current weight limit,
			// we can't include this item in the knapsack.
			if items[i-1].Weight > w {
				knapsack[i][w] = knapsack[i-1][w]
			} else {
				// Otherwise, we check whether to include the current item
				// or not, based on which option gives the maximum value.
				withoutCurrent := knapsack[i-1][w]
				withCurrent := items[i-1].Value + knapsack[i-1][w-items[i-1].Weight]
				knapsack[i][w] = max(withoutCurrent, withCurrent)
			}
		}
	}

	// Return the maximum value that can be obtained with the given items
	// and maximum weight limit.
	return knapsack[amountOfItems][maximumWeight]
}

// max returns the greater of two integers a and b.
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

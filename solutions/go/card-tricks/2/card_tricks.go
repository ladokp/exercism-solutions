package cards

// GetItem retrieves an item from a slice at given position. The second return value indicates whether
// the given index exists in the slice or not.
func GetItem(slice []int, index int) int {
	if index < 0 || index > len(slice)-1 {
		return -1
	}
	return slice[index]
}

// SetItem writes an item to a slice at given position overwriting an existing value.
// If the index is out of range the value needs to be appended.
func SetItem(slice []int, index, value int) []int {
	if index < 0 || index > len(slice)-1 {
		slice = append(slice, value)
	} else {
		slice[index] = value
	}
	return slice
}

// PrependItems prepends items to a given slice
func PrependItems(slice []int, value ...int) []int {
	return append(value, slice...)
}

// RemoveItem removes an item from a slice by modifying the existing slice.
func RemoveItem(slice []int, index int) []int {
	if index <= len(slice)-1 && index >= 0 {
		slice = append(slice[:index], slice[index+1:]...)
	}
	return slice
}

// FavoriteCards returns a specific slice of cards
func FavoriteCards() []int {
	return []int{2, 6, 9}
}

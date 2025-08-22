package binarysearch

func SearchInts(input []int, key int) int {
	return binarySearch(input, key, 0, len(input)-1)
}

func binarySearch(arr []int, key int, left int, right int) int {
	if left > right {
		return -1
	}
	mid := (left + right) / 2
	switch {
	case arr[mid] == key:
		return mid
	case arr[mid] < key:
		return binarySearch(arr, key, mid+1, right)
	default: // arr[mid] > key
		return binarySearch(arr, key, left, mid-1)
	}
}

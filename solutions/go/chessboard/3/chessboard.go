package chessboard

// File which stores if a square is occupied by a piece - this will be a slice of bools
type File []bool

// Chessboard contains a map of eight Ranks, accessed with values from "A" to "H"
type Chessboard map[string]File

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank
func CountInRank(cb Chessboard, rank int) int {
	var count = 0
	if rank > 0 && rank < 9 {
		for _, file := range cb {
			if file[rank-1] {
				count += 1
			}
		}
	}
	return count
}

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file
func CountInFile(cb Chessboard, file string) int {
	var count = 0
	fileToCount, found := cb[file]
	if found {
		for _, square := range fileToCount {
			if square {
				count += 1
			}
		}
	}
	return count
}

// CountAll should count how many squares are present in the chessboard
func CountAll(cb Chessboard) int {
	var count = 0
	for _, value := range cb {
		for i := 0; i < len(value); i += 1 {
			count += 1
		}
	}
	return count
}

// CountOccupied returns how many squares are occupied in the chessboard
func CountOccupied(cb Chessboard) int {
	var count = 0
	for file := range cb {
		count += CountInFile(cb, file)
	}
	return count
}

package chessboard

// Declare a type named Rank which stores if a square is occupied by a piece - this will be a slice of bools
type Rank []bool

// Declare a type named Chessboard contains a map of eight Ranks, accessed with values from "A" to "H"
type Chessboard map[string]Rank

// CountInRank returns how many squares are occupied in the chessboard,
// within the given rank
func CountInRank(cb Chessboard, rank string) int {
    var count = 0
	for key, value := range cb {
        if key == rank {
            for i := 0; i < len(value); i += 1 {
                if value[i] {
                    count += 1
                }
            }
        	return count
        }
    }
	return count
}

// CountInFile returns how many squares are occupied in the chessboard,
// within the given file
func CountInFile(cb Chessboard, file int) int {
    var count = 0
	if file >= 0 && file <= 8 {
    	for _, rank := range cb {
            if rank[file-1] {
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
	for _, value := range cb {
        for i := 0; i < len(value); i += 1 {
            if value[i] {
                count += 1
            }
        }
    }
	return count
}

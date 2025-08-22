package birdwatcher

// TotalBirdCount return the total bird count by summing
// the individual day's counts.
func TotalBirdCount(birdsPerDay []int) int {
	var birdCount = 0
    for index := 0; index < len(birdsPerDay); index++ {
        birdCount += birdsPerDay[index]
    }
	return birdCount
}

// BirdsInWeek returns the total bird count by summing
// only the items belonging to the given week.
func BirdsInWeek(birdsPerDay []int, week int) int {
    return TotalBirdCount(birdsPerDay[7*(week-1):7*week])
}

// FixBirdCountLog returns the bird counts after correcting
// the bird counts for alternate days.
func FixBirdCountLog(birdsPerDay []int) []int {
    for index := 0; index < len(birdsPerDay); index += 2 {
        birdsPerDay[index] = birdsPerDay[index] +1
    }
	return birdsPerDay
}

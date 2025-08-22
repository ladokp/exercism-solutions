package cars

func CalculateWorkingCarsPerHour(cars int, successRate float64) float64 {
	return float64(cars) * successRate / 100
}

func CalculateWorkingCarsPerMinute(cars int, successRate float64) int {
	return int(CalculateWorkingCarsPerHour(cars, successRate) / 60)
}

func CalculateCost(cars int) uint {
	return uint((cars/10)*95000 + (cars%10)*10000)
}

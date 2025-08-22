package series

func All(number int, series string) (result []string) {
	for index := 0; index < len(series)-number+1; index++ {
		result = append(result, series[index:index+number])
	}
	return
}

func UnsafeFirst(number int, series string) (result string) {
	if len(series) >= number {
		result = series[0:number]
	}
	return
}

func First(number int, series string) (result string, ok bool) {
	result = UnsafeFirst(number, series)
	ok = len(result) == number
	return
}

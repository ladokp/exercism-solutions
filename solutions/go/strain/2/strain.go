package strain

func Keep[T any](list []T, filter func(T) bool) []T {
	result := []T{}
	for _, element := range list {
		if filter(element) {
			result = append(result, element)
		}
	}
	return result
}

func Discard[T any](list []T, filter func(T) bool) []T {
	return Keep(list, func(element T) bool { return !filter(element) })
}

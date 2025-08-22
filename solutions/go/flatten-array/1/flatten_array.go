package flatten

func Flatten(nested interface{}) []interface{} {
	result := make([]interface{}, 0)
	switch element := nested.(type) {
	case []interface{}:
		for _, sublist := range element {
			result = append(result, Flatten(sublist)...)
		}
	case interface{}:
		result = append(result, element)
	}
	return result
}

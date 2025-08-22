package listops

type binFunc func(int, int) int
type predFunc func(int) bool
type unaryFunc func(int) int
type IntList []int

func (list IntList) Foldr(function binFunc, initial int) int {
	for index := list.Length() - 1; index >= 0; index-- {
		initial = function(list[index], initial)
	}
	return initial
}
func (list IntList) Foldl(function binFunc, initial int) int {
	for _, value := range list {
		initial = function(initial, value)
	}
	return initial
}
func (list IntList) Filter(function predFunc) IntList {
	result := IntList{}
	for _, value := range list {
		if function(value) {
			result = result.Append(IntList{value})
		}
	}
	return result
}
func (list IntList) Length() int {
	return list.Foldl(func(x, y int) int { return x + 1 }, 0)
}
func (list IntList) Map(function unaryFunc) IntList {
	result := IntList{}
	for _, value := range list {
		result = result.Append(IntList{function(value)})
	}
	return result
}
func (list IntList) Reverse() IntList {
	result := IntList{}
	for index := list.Length() - 1; index >= 0; index-- {
		result = result.Append(IntList{list[index]})
	}
	return result
}
func (list IntList) Append(secondList IntList) IntList {
	result := make(IntList, len(list)+len(secondList))
	index := 0
	for ; index < len(list); index++ {
		result[index] = list[index]
	}
	for index2 := 0; index2 < len(secondList); index2++ {
		result[index] = secondList[index2]
		index++
	}
	return result
}
func (list IntList) Concat(lists []IntList) IntList {
	result := IntList{}.Append(list)
	for _, list := range lists {
		result = result.Append(list)
	}
	return result
}

package strain

type Ints []int
type Lists [][]int
type Strings []string

func (i Ints) Keep(filter func(int) bool) (output Ints) {
	for _, number := range i {
		if filter(number) {
			output = append(output, number)
		}
	}
	return
}

func (i Ints) Discard(filter func(int) bool) Ints {
	return i.Keep(func(number int) bool { return !filter(number) })
}

func (l Lists) Keep(filter func([]int) bool) (output Lists) {
	for _, number := range l {
		if filter(number) {
			output = append(output, number)
		}
	}
	return
}

func (s Strings) Keep(filter func(string) bool) (output Strings) {
	for _, word := range s {
		if filter(word) {
			output = append(output, word)
		}
	}
	return
}

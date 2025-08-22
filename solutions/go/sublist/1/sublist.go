package sublist

import "reflect"

// Relation type is defined in relations.go file.

func Sublist(l1, l2 []int) Relation {
	isL1Bigger := false
	bigger := l2
	smaller := l1
	if reflect.DeepEqual(l1, l2) {
		return RelationEqual
	} else if len(l1) > len(l2) {
		isL1Bigger = true
		bigger = l1
		smaller = l2
	}
	for index := 0; index < len(bigger)-len(smaller)+1; index++ {
		areListsEqual := reflect.DeepEqual(bigger[index:index+len(smaller)], smaller)
		if areListsEqual && isL1Bigger {
			return RelationSuperlist
		}
		if areListsEqual && !isL1Bigger {
			return RelationSublist
		}
	}
	return RelationUnequal
}

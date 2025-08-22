package stringset

import (
	"fmt"
	"strings"
)

// Set is a collection of unique string values.
type Set map[string]bool

func New() Set {
	return Set{}
}

func (set Set) Length() int {
	return len(set)
}

func NewFromSlice(list []string) Set {
	set := New()
	for _, elem := range list {
		set.Add(elem)
	}
	return set
}

func (set Set) Slice() []string {
	slice := make([]string, 0, set.Length())
	for key, _ := range set {
		slice = append(slice, key)
	}
	return slice
}

func (set Set) String() string {
	quotedStrings := set.Slice()
	for i, element := range quotedStrings {
		quotedStrings[i] = fmt.Sprintf("%q", element)
	}
	return fmt.Sprintf("{%s}", strings.Join(quotedStrings, ", "))
}

func (set Set) IsEmpty() bool {
	return set.Length() == 0
}

func (set Set) Has(elem string) bool {
	_, found := set[elem]
	return found
}

func (set Set) Add(elem string) {
	set[elem] = true
}

func Subset(s1, s2 Set) bool {
	for key, _ := range s1 {
		_, found := s2[key]
		if !found {
			return false
		}
	}
	return true
}

func Disjoint(s1, s2 Set) bool {
	for key, _ := range s1 {
		_, found := s2[key]
		if found {
			return false
		}
	}
	return true
}

func Equal(s1, s2 Set) bool {
	if len(s1) != len(s2) {
		return false
	}
	return Subset(s1, s2)
}

func Intersection(s1, s2 Set) Set {
	intersection := New()
	for key, _ := range s2 {
		_, found := s1[key]
		if found {
			intersection.Add(key)
		}
	}
	return intersection
}

func Difference(s1, s2 Set) Set {
	difference := New()
	for key, _ := range s1 {
		_, found := s2[key]
		if !found {
			difference.Add(key)
		}
	}
	return difference
}

func Union(s1, s2 Set) Set {
	union := NewFromSlice(s1.Slice())
	for key, _ := range s2 {
		union.Add(key)
	}
	return union
}

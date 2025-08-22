package school

import (
	"sort"
)

type Grade struct {
	level int
	names []string
}

type School struct{ roster map[int]*Grade }

func New() *School {
	return &School{make(map[int]*Grade)}
}

func (school *School) Add(student string, level int) {
	if grade, found := school.roster[level]; found {
		grade.names = append(grade.names, student)
		sort.Strings(grade.names)
	} else {
		school.roster[level] = &Grade{level, []string{student}}
	}
}

func (school *School) Grade(level int) []string {
	if grade, found := school.roster[level]; found {
		return grade.names
	}
	return nil
}

func (school *School) Enrollment() (grades []Grade) {
	keys := make([]int, 0, len(school.roster))
	for key := range school.roster {
		keys = append(keys, key)
	}
	sort.Ints(keys)
	for _, key := range keys {
		grades = append(grades, *school.roster[key])
	}
	return
}

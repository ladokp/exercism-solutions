package school

import (
	"sort"
)

type Grade struct {
	level    int
	students []string
}

type School struct{ grades map[int]*Grade }

func New() *School {
	return &School{make(map[int]*Grade)}
}

func (s *School) Add(student string, level int) {
	if grade, found := s.grades[level]; found {
		grade.students = append(grade.students, student)
		sort.Strings(grade.students)
	} else {
		s.grades[level] = &Grade{level, []string{student}}
	}

}

func (s *School) Grade(level int) []string {
	grade, found := s.grades[level]
	if found {
		return grade.students
	}
	return make([]string, 0)
}

func (s *School) Enrollment() (grades []Grade) {
	keys := make([]int, 0, len(s.grades))
	for k := range s.grades {
		keys = append(keys, k)
	}
	sort.Ints(keys)
	for _, key := range keys {
		grades = append(grades, *s.grades[key])
	}
	return
}

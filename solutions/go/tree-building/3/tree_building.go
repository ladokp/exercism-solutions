package tree

import (
	"errors"
	"sort"
)

// The Record type describes a node line from a database
type Record struct {
	ID     int
	Parent int
}

// The Node type describes a tree structure
type Node struct {
	ID       int
	Children []*Node
}

func countRoots(records []Record) int {
	var counter int
	for _, r := range records {
		if r.ID == r.Parent {
			counter++
		}
	}
	return counter
}

func countNodes(records []Record, id int) int {
	var counter int
	for _, r := range records {
		if r.ID == id {
			counter++
		}
	}
	return counter
}

func makeTree(records []Record, parentId int) ([]*Node, error) {
	var children []*Node
	for _, r := range records {
		if countNodes(records, r.ID) > 1 {
			return nil, errors.New("duplicate node")
		}
		if r.ID == r.Parent {
			continue
		} else if r.Parent == parentId {
			var currentChildren, error = makeTree(records, r.ID)
			if error != nil {
				return nil, error
			}
			children = append(children, &Node{r.ID, currentChildren})
		} else if r.Parent > r.ID {
			return nil, errors.New("parent > ID")
		}

	}
	sort.SliceStable(children, func(i, j int) bool {
		return children[i].ID < children[j].ID
	})
	return children, nil
}

func Build(records []Record) (*Node, error) {
	if len(records) == 0 {
		return nil, nil
	}
	if countRoots(records) != 1 {
		return nil, errors.New("no or more than one root node found")
	}
	for _, r := range records {
		if r.ID < 0 || r.ID >= len(records) {
			return nil, errors.New("record out of bounds")
		} else if r.ID == r.Parent {
			var currentChildren, error = makeTree(records, r.ID)
			if error != nil {
				return nil, error
			}
			return &Node{r.ID, currentChildren}, nil
		}
	}
	return nil, errors.New("no root node found")
}

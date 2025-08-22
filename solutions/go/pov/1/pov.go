package pov

type Tree struct {
	value    string
	children []*Tree
	parent   *Tree
}

func New(value string, children ...*Tree) *Tree {
	if value == "" {
		return nil
	}
	result := &Tree{value, children, nil}
	for _, child := range children {
		if child != nil {
			child.parent = result
		}
	}
	return result
}

func (tree *Tree) Value() string {
	if tree == nil {
		return ""
	} else {
		return tree.value
	}
}

func (tree *Tree) Children() []*Tree {
	if tree == nil {
		return nil
	} else {
		return tree.children
	}
}

func (tree *Tree) FindNode(value string) *Tree {
	if tree == nil || tree.value == value {
		return tree
	}
	for _, child := range tree.children {
		result := child.FindNode(value)
		if result != nil {
			return result
		}
	}
	return nil
}

func (tree *Tree) FromPov(from string) *Tree {
	node := tree.FindNode(from)
	seen := make(map[string]bool)
	var f func(*Tree) *Tree
	f = func(tr *Tree) *Tree {
		if tr == nil || seen[tr.value] {
			return nil
		}
		seen[tr.value] = true
		children := make([]*Tree, 0, len(tr.children)+1)
		for _, child := range tr.children {
			fChild := f(child)
			if fChild != nil {
				children = append(children, fChild)
			}
		}
		if tr.parent != nil {
			fParent := f(tr.parent)
			if fParent != nil {
				children = append(children, fParent)
			}
		}
		return New(tr.value, children...)
	}
	return f(node)
}

func (tree *Tree) PathFromRoot(value string) []string {
	node := tree.FindNode(value)
	if node == nil {
		return nil
	}
	result := make([]string, 0)
	for node != tree {
		result = append(result, node.value)
		node = node.parent
	}
	result = append(result, tree.value)
	for i, j := 0, len(result)-1; i < j; i, j = i+1, j-1 {
		result[i], result[j] = result[j], result[i]
	}
	return result
}

func (tree *Tree) PathTo(from, to string) []string {
	if tree == nil {
		return nil
	}
	if from == to {
		return []string{to}
	}
	tree1 := tree.FromPov(from)
	if tree1 == nil {
		return nil
	}
	return tree1.PathFromRoot(to)
}

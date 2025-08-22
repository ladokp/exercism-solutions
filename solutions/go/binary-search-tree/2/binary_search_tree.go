package binarysearchtree

type BinarySearchTree struct {
	left  *BinarySearchTree
	data  int
	right *BinarySearchTree
}

// NewBst creates and returns a new BinarySearchTree.
func NewBst(data int) *BinarySearchTree {
	return &BinarySearchTree{data: data}
}

// Insert inserts an int into the BinarySearchTree.
// Inserts happen based on the rules of a binary search tree
func (bst *BinarySearchTree) Insert(data int) {
	if bst.data >= data {
		if bst.left == nil {
			bst.left = NewBst(data)
		} else {
			bst.left.Insert(data)
		}
	} else {
		if bst.right == nil {
			bst.right = NewBst(data)
		} else {
			bst.right.Insert(data)
		}
	}
}

// SortedData returns the ordered contents of BinarySearchTree as an []int.
// The values are in increasing order starting with the lowest int value.
// A BinarySearchTree that has the numbers [1,3,7,5] added will return the
// []int [1,3,5,7].
func (bst *BinarySearchTree) SortedData() (returnSlice []int) {
	if bst.left != nil {
		returnSlice = append(returnSlice, bst.left.SortedData()...)
	}
	returnSlice = append(returnSlice, bst.data)
	if bst.right != nil {
		returnSlice = append(returnSlice, bst.right.SortedData()...)
	}
	return
}

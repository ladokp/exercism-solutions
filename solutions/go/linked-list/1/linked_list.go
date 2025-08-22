package linkedlist

import "errors"

type List struct {
	first, last *Node
}

type Node struct {
	Value      interface{}
	prev, next *Node
}

func NewList(args ...interface{}) *List {
	list := new(List)
	for _, value := range args {
		list.Push(value)
	}
	return list
}

func (n *Node) Next() *Node {
	return n.next
}

func (n *Node) Prev() *Node {
	return n.prev
}

func (l *List) Unshift(v interface{}) {
	second := l.first
	l.first = &Node{v, nil, second}
	if second == nil {
		l.last = l.first
	} else {
		second.prev = l.first
	}
}

func (l *List) Push(v interface{}) {
	penultimate := l.last
	l.last = &Node{v, l.last, nil}
	if penultimate == nil {
		l.first = l.last
	} else {
		penultimate.next = l.last
	}
}

func (l *List) Shift() (interface{}, error) {
	if l.first == nil {
		return nil, errors.New("List is empty.")
	}
	value := l.first.Value
	second := l.first.Next()
	l.first.next = nil
	l.first = second
	if second == nil {
		l.last = nil
	} else {
		second.prev = nil
	}
	return value, nil
}

func (l *List) Pop() (interface{}, error) {
	if l.last == nil {
		return nil, errors.New("List is empty.")
	}
	value := l.last.Value
	penultimate := l.last.Prev()
	l.last.prev = nil
	l.last = penultimate
	if penultimate == nil {
		l.first = nil
	} else {
		penultimate.next = nil
	}
	return value, nil
}

func (l *List) Reverse() {
	reversed := List{}
	for {
		if value, error := l.Pop(); error != nil {
			break
		} else {
			reversed.Push(value)
		}
	}
	l.first = reversed.first
	l.last = reversed.last
}

func (l *List) First() *Node {
	return l.first
}

func (l *List) Last() *Node {
	return l.last
}

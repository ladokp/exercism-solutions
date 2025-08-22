// Package twofer creates a string to share something between to persons
package twofer

import "fmt"

// ShareWith generates a string to share something between two persons
func ShareWith(name string) string {
	if name == "" {
		name = "you"
	}
	return fmt.Sprintf("One for %s, one for me.", name)
}
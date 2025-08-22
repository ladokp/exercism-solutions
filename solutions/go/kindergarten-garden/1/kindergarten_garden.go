package kindergarten

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

type Garden struct {
	PlantsMap map[string][]string
}

func convertPlantShortcut(shortcut string) (string, bool) {
	switch shortcut {
	case "G":
		return "grass", true
	case "C":
		return "clover", true
	case "R":
		return "radishes", true
	case "V":
		return "violets", true
	}
	return "", false
}

func NewGarden(diagram string, children []string) (*Garden, error) {
	rows := strings.Split(diagram, "\n")
	if len(rows) != 3 {
		return nil, fmt.Errorf("expected 3 lines in diagram bug got %v", len(rows))
	}
	rows = rows[1:]
	if len(rows[0]) != 2*len(children) || len(rows[1]) != 2*len(children) {
		return nil, errors.New("number of plants in row is illegal")
	}

	plantMap := map[string][]string{}
	orderedChildren := make([]string, len(children))
	copy(orderedChildren, children)
	sort.Sort(sort.StringSlice(orderedChildren))

	for index, name := range orderedChildren {
		currentList := plantMap[name]
		for _, row := range []int{0, 1} {
			for _, rowIndex := range []int{index * 2, index*2 + 1} {
				plant, ok := convertPlantShortcut(string(rows[row][rowIndex]))
				if !ok {
					return nil, errors.New("one or more cups have invalid codes")
				}
				currentList = append(currentList, plant)
			}
		}
		plantMap[name] = currentList
	}

	if len(plantMap) != len(children) {
		return nil, errors.New("duplicate name found in children names")
	}

	return &Garden{plantMap}, nil
}

func (g *Garden) Plants(child string) ([]string, bool) {
	if _, ok := g.PlantsMap[child]; ok {
		return g.PlantsMap[child], true
	}
	return nil, false
}

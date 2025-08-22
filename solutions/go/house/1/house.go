package house

import "strings"

type Actor struct {
	name   string
	action string
}

var actors = map[int]Actor{
	1:  {name: "house that Jack built", action: "lay in"},
	2:  {name: "malt", action: "ate"},
	3:  {name: "rat", action: "killed"},
	4:  {name: "cat", action: "worried"},
	5:  {name: "dog", action: "tossed"},
	6:  {name: "cow with the crumpled horn", action: "milked"},
	7:  {name: "maiden all forlorn", action: "kissed"},
	8:  {name: "man all tattered and torn", action: "married"},
	9:  {name: "priest all shaven and shorn", action: "woke"},
	10: {name: "rooster that crowed in the morn", action: "kept"},
	11: {name: "farmer sowing his corn", action: "belonged to"},
	12: {name: "horse and the hound and the horn", action: ""},
}

func Verse(v int) string {
	var sb strings.Builder
	sb.WriteString("This is the ")
	sb.WriteString(actors[v].name)
	for i := v - 1; i > 0; i-- {
		sb.WriteString("\nthat ")
		sb.WriteString(actors[i].action)
		sb.WriteString(" the ")
		sb.WriteString(actors[i].name)
	}
	sb.WriteString(".")
	return sb.String()
}

func Song() string {
	var sb strings.Builder
	for i := 1; i <= len(actors); i++ {
		sb.WriteString(Verse(i))
		if i != len(actors) {
			sb.WriteString("\n\n")
		}
	}
	return sb.String()
}

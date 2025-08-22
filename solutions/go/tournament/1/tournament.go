package tournament

import (
	"bufio"
	"fmt"
	"io"
	"sort"
	"strings"
)

type Team struct {
	name   string
	played int
	won    int
	lost   int
	drawn  int
}

type Teams map[string]*Team

func (t *Team) win() {
	t.played++
	t.won++
}

func (t *Team) lose() {
	t.played++
	t.lost++
}

func (t *Team) draw() {
	t.played++
	t.drawn++
}

func (t *Team) points() int {
	return 3*t.won + 1*t.drawn
}

func (t Teams) getOrCreateTeam(n string) *Team {
	if t[n] == nil {
		t[n] = &Team{name: n}
	}

	return t[n]
}

func (t Teams) sort() []*Team {
	var all []*Team
	for _, team := range t {
		all = append(all, team)
	}

	sort.Slice(all, func(i, j int) bool {
		if all[i].points() == all[j].points() {
			return all[i].name < all[j].name
		}

		return all[i].points() > all[j].points()
	})

	return all
}

func (t Teams) fromResults(r io.Reader) error {
	s := bufio.NewScanner(r)

	for s.Scan() {
		l := s.Text()
		if l == "" || l[0] == '#' {
			continue
		}

		chunks := strings.Split(l, ";")
		if len(chunks) != 3 {
			return fmt.Errorf("wrong field count for line: %s (got %d fields)", l, len(chunks))
		}

		t1, t2, outcome := chunks[0], chunks[1], chunks[2]
		switch outcome {
		case "win":
			t.getOrCreateTeam(t1).win()
			t.getOrCreateTeam(t2).lose()
		case "loss":
			t.getOrCreateTeam(t1).lose()
			t.getOrCreateTeam(t2).win()
		case "draw":
			t.getOrCreateTeam(t1).draw()
			t.getOrCreateTeam(t2).draw()
		default:
			return fmt.Errorf("invalid outcome %s", outcome)
		}
	}

	return nil
}

// Tally reads a newline-separated list of match outcomes from r and writes
// a summary of the result to w
func Tally(r io.Reader, w io.Writer) error {
	t := make(Teams)
	err := t.fromResults(r)
	if err != nil {
		return err
	}

	_, err = fmt.Fprintf(w, "%-30s | %2s | %2s | %2s | %2s | %2s\n", "Team", "MP", "W", "D", "L", "P")
	if err != nil {
		return err
	}

	for _, team := range t.sort() {
		_, err := fmt.Fprintf(w, "%-30s | %2d | %2d | %2d | %2d | %2d\n", team.name, team.played, team.won, team.drawn, team.lost, team.points())

		if err != nil {
			return err
		}
	}

	return nil
}

package meetup

import "time"

type WeekSchedule int

const (
	First = WeekSchedule(iota)
	Second
	Third
	Fourth
	Last
	Teenth
)

func Day(schedule WeekSchedule, day time.Weekday, month time.Month, year int) int {
	date := time.Date(year, month, 1, 0, 0, 0, 0, time.FixedZone("UTC", 0))
	var matches []int
	for date.Month() == month {
		if date.Weekday() == day {
			matches = append(matches, date.Day())
		}
		date = date.AddDate(0, 0, 1) // 1 day
	}
	switch schedule {
	case First:
		return matches[0]
	case Second:
		return matches[1]
	case Third:
		return matches[2]
	case Fourth:
		return matches[3]
	case Last:
		return matches[len(matches)-1]
	case Teenth:
		for _, d := range matches {
			if d >= 13 {
				return d
			}
		}
	}
	panic("Invalid schedule")
}

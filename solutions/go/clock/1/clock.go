package clock

import "fmt"

const DailyHours = 24
const HourlyMinutes = 60
const DailyMinutes = DailyHours * HourlyMinutes

type Clock struct {
	min int
}

func New(h, m int) Clock {
	c := Clock{(HourlyMinutes*h + m) % DailyMinutes}
	if c.min < 0 {
		c.min += DailyMinutes
	}
	return c
}

func (c Clock) Add(m int) Clock {
	c.min = (c.min + m) % DailyMinutes
	if c.min < 0 {
		c.min += DailyMinutes
	}
	return c
}

func (c Clock) Subtract(m int) Clock {
	return c.Add(-m)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.min/HourlyMinutes, c.min%HourlyMinutes)
}

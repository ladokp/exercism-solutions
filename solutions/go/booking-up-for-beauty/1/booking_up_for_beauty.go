package booking

import "time"
import "fmt"

// Schedule returns a time.Time from a string containing a date
func Schedule(date string) time.Time {
    var time_, _ = time.Parse("1/2/2006 15:04:05", date)
	return time_
}

// HasPassed returns whether a date has passed
func HasPassed(date string) bool {
    var time_, _ = time.Parse("January 2, 2006 15:04:05", date)
    return time.Since(time_) > 0
}

// IsAfternoonAppointment returns whether a time is in the afternoon
func IsAfternoonAppointment(date string) bool {
    var time_, _ = time.Parse("Monday, January 2, 2006 15:04:05", date)
    return time_.Hour() >= 12 && time_.Hour() <= 18
}

// Description returns a formatted string of the appointment time
func Description(date string) string {
    var time_, _ = time.Parse("1/2/2006 15:04:05", date)
    var dateString = time_.Format("Monday, January 2, 2006")
    var timeString = time_.Format("15:04")
	return fmt.Sprintf("You have an appointment on %s, at %s.", dateString, timeString)
}

// AnniversaryDate returns a Time with this year's anniversary
func AnniversaryDate() time.Time {
    var time_ = time.Now()
	return time.Date(time_.Year(), 9, 15, 0, 0, 0, 0, time.UTC)
}

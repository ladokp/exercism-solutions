package secret

const testVersion = 2

var values = []string{"wink", "double blink", "close your eyes", "jump"}

func Handshake(code uint) []string {
	events := make([]string, 0)
	for index := 0; index < len(values); index++ {
		if code&uint(1<<uint(index)) > 0 {
			events = append(events, values[index])
		}
	}
	if code&16 >= 1 {
		for index := len(events)/2 - 1; index >= 0; index-- {
			oppositeIndex := len(events) - 1 - index
			events[index], events[oppositeIndex] = events[oppositeIndex], events[index]
		}
	}
	return events
}

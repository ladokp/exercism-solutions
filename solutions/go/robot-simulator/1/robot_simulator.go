package robot

const (
	N Dir = iota
	E
	S
	W
)

func (dir Dir) String() (direction string) {
	switch dir {
	case N:
		direction = "North"
	case E:
		direction = "East"
	case S:
		direction = "South"
	case W:
		direction = "West"
	}
	return
}

func Advance() {
	switch Step1Robot.Dir {
	case N:
		Step1Robot.Y++
	case E:
		Step1Robot.X++
	case S:
		Step1Robot.Y--
	case W:
		Step1Robot.X--
	}
}

func Right() {
	Step1Robot.Dir = (Step1Robot.Dir + 1) % 4
}

func Left() {
	Step1Robot.Dir = (Step1Robot.Dir + 3) % 4
}

func (robot *Step2Robot) IsOutsideRoom(rect Rect) bool {
	return !(robot.Pos.Northing >= rect.Min.Northing && robot.Pos.Northing <= rect.Max.Northing) ||
		!(robot.Pos.Easting >= rect.Min.Easting && robot.Pos.Easting <= rect.Max.Easting)
}

func (robot *Step2Robot) Advance(rect Rect) bool {
	switch robot.Dir {
	case N:
		if robot.Pos.Northing < rect.Max.Northing {
			robot.Pos.Northing++
			return true
		}
	case E:
		if robot.Pos.Easting < rect.Max.Easting {
			robot.Pos.Easting++
			return true
		}
	case S:
		if robot.Pos.Northing > rect.Min.Northing {
			robot.Pos.Northing--
			return true
		}
	case W:
		if robot.Pos.Easting > rect.Min.Easting {
			robot.Pos.Easting--
			return true
		}
	}
	return false
}

func (robot *Step2Robot) TurnLeft() {
	robot.Dir = (robot.Dir - 1 + 4) % 4
}

func (robot *Step2Robot) TurnRight() {
	robot.Dir = (robot.Dir + 1) % 4
}

type Action uint8

const (
	advance Action = iota
	turnLeft
	turnRight
	completed
	badCommand
)

func StartRobot(cmdChan <-chan Command, actChan chan<- Action) {
	for cmd := range cmdChan {
		switch cmd {
		case 'A':
			actChan <- advance
		case 'L':
			actChan <- turnLeft
		case 'R':
			actChan <- turnRight
		}
	}
	actChan <- completed
}

func Room(rect Rect, robot Step2Robot, actChan <-chan Action, rep chan<- Step2Robot) {
	for act := range actChan {
		switch act {
		case advance:
			robot.Advance(rect)
		case turnLeft:
			robot.TurnLeft()
		case turnRight:
			robot.TurnRight()
		case completed:
			rep <- robot
			return
		}
	}
}

type Action3 struct {
	name   string
	action Action
}

func StartRobot3(name string, scr string, actChan chan<- Action3, log chan<- string) {
	if name == "" {
		log <- "A robot without name"
		return
	}

	for _, b := range []byte(scr) {
		switch b {
		case 'R':
			actChan <- Action3{name, turnRight}
		case 'L':
			actChan <- Action3{name, turnLeft}
		case 'A':
			actChan <- Action3{name, advance}
		default:
			actChan <- Action3{name, badCommand}
			return
		}
	}
	actChan <- Action3{name, completed}
}

func Room3(rect Rect, robots []Step3Robot, actChan <-chan Action3, rep chan<- []Step3Robot, log chan<- string) {
	defer close(rep)

	posRecords := map[Pos]bool{}
	idRecords := map[string]int{}
	for i, robot := range robots {
		if robot.Name == "" {
			return
		} else if idRecords[robot.Name] > 0 {
			log <- "Duplicate robot names"
			return
		} else if posRecords[robot.Pos] {
			log <- "Robots placed at the same place"
			return
		} else if robot.IsOutsideRoom(rect) {
			log <- "A robot placed outside of the room"
			return
		}
		idRecords[robot.Name] = i + 1
		posRecords[robot.Pos] = true
	}

	for act := range actChan {
		id := idRecords[act.name]
		if id < 1 {
			log <- "An action from unknown robot"
			return
		}
		switch act.action {
		case advance:
			robot := robots[id-1].Step2Robot
			oldPos := robot.Pos
			if !robot.Advance(rect) {
				log <- "A robot attempting to advance into a wall"
			} else if posRecords[robot.Pos] {
				log <- "A robot attempting to advance into another robot"
			} else {
				posRecords[oldPos] = false
				posRecords[robot.Pos] = true
				robots[id-1].Step2Robot = robot
			}
		case turnLeft:
			robots[id-1].Step2Robot.TurnLeft()
		case turnRight:
			robots[id-1].Step2Robot.TurnRight()
		case completed:
			if delete(idRecords, act.name); len(idRecords) == 0 {
				rep <- robots
				return
			}
		case badCommand:
			log <- "An undefined command in a script"
			return
		}
	}
}

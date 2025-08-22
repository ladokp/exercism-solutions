package jedlik

import "fmt"

// TODO: define the 'Drive()' method
func (car *Car) Drive() {
    if car.CanFinish(car.speed) {
        car.distance += car.speed
        car.battery -= car.batteryDrain
    }
}

// TODO: define the 'DisplayDistance() string' method
func (car *Car) DisplayDistance() string {
    return fmt.Sprintf("Driven %d meters", car.distance)
}

// TODO: define the 'DisplayBattery() string' method
func (car *Car) DisplayBattery() string {
    return fmt.Sprintf("Battery at %d", car.battery) + "%"
}

// TODO: define the 'CanFinish(trackDistance int) bool' method
func (car *Car) CanFinish(trackDistance int) bool {
    return (car.batteryDrain * trackDistance / car.speed) <= car.battery
}

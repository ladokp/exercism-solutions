package space

import "strings"

var planets = map[Planet]float64{
	"earth":   1.0,
	"mercury": 0.2408467,
	"venus":   0.61519726,
	"mars":    1.8808158,
	"jupiter": 11.862615,
	"saturn":  29.447498,
	"uranus":  84.016846,
	"neptune": 164.79132}

type Planet string

func Age(seconds float64, planet Planet) float64 {
	if coefficient, OK := planets[Planet(strings.ToLower(string(planet)))]; OK {
		return seconds / (31557600.0 * coefficient)
	}
	return -1
}

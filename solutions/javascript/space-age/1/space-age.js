const planets = {
    "earth": 1.0,
    "mercury": 0.2408467,
    "venus": 0.61519726,
    "mars": 1.8808158,
    "jupiter": 11.862615,
    "saturn": 29.447498,
    "uranus": 84.016846,
    "neptune": 164.79132
};

function age(planet, seconds) {
    const coefficient = planets[planet.toLowerCase()];
    if (coefficient !== undefined) {
        return parseFloat((seconds / (31557600.0 * coefficient)).toFixed(2));
    }
    return -1;
}

// Export the function for use in other modules
export { age };

"""
This module provides functionality to represent and work with musical scales.

It defines constants for sharp and flat scales, a set to determine if a
scale uses sharps, and steps for intervals in semitones. The primary
class is `Scale`, which allows you to generate chromatic scales and
various scale types based on a tonic note and specified intervals.
"""

SHARP_SCALE = (
    "A",
    "A#",
    "B",
    "C",
    "C#",
    "D",
    "D#",
    "E",
    "F",
    "F#",
    "G",
    "G#",
)
FLAT_SCALE = (
    "A",
    "Bb",
    "B",
    "C",
    "Db",
    "D",
    "Eb",
    "E",
    "F",
    "Gb",
    "G",
    "Ab",
)
USES_SHARPS = {
    "C",
    "a",
    "G",
    "D",
    "A",
    "E",
    "B",
    "F#",
    "e",
    "b",
    "f#",
    "c#",
    "g#",
    "d#",
}
STEPS = {
    "m": 1,
    "M": 2,
    "A": 3,
}


class Scale:
    """Class to represent a musical scale with a given tonic.

    Attributes:
        tonic (str): The tonic note of the scale.
        scale (tuple): The chromatic scale (sharp or flat) used.
        chromatic_scale (list): Pre-computed chromatic scale from the tonic.
    """

    def __init__(self, tonic):
        """Initialize the Scale with a tonic note and determine its scale type.

        Args:
            tonic (str): The tonic note of the scale.
        """
        self.scale = SHARP_SCALE if tonic in USES_SHARPS else FLAT_SCALE
        self.tonic = tonic.capitalize()
        self.chromatic_scale = self.generate_chromatic_scale()

    def generate_chromatic_scale(self):
        """Generates the chromatic scale starting from the tonic note.

        Returns:
            list: Chromatic scale starting from the given tonic.
        """
        start_index = self.scale.index(self.tonic)
        return [self.scale[(start_index + index) % 12] for index in range(12)]

    def chromatic(self):
        """Return the pre-computed chromatic scale.

        Returns:
            list: Chromatic scale starting from the tonic note.
        """
        return self.chromatic_scale

    def interval(self, intervals):
        """Return the scale notes based on the specified intervals.

        Args:
            intervals (str): A string of interval steps using 'm' for minor,
                             'M' for major, and 'A' for augmented.

        Returns:
            list: Scale notes based on the specified intervals.
        """
        index, scale = 0, [self.chromatic_scale[0]]
        for step in intervals:
            index += STEPS[step]
            scale.append(self.chromatic_scale[index % 12])
        return scale

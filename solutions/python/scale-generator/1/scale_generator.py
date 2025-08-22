SHARP_SCALE = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
FLAT_SCALE = ["A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"]
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
STEPS = {"m": 1, "M": 2, "A": 3}


class Scale:
    def __init__(self, tonic):
        self.scale = SHARP_SCALE if tonic in USES_SHARPS else FLAT_SCALE
        self.tonic = tonic.capitalize()

    def chromatic(self):
        return [
            self.scale[(self.scale.index(self.tonic) + i) % 12]
            for i in range(12)
        ]

    def interval(self, intervals):
        chromatic, index = self.chromatic(), 0
        scale = [chromatic[index]]
        for step in intervals:
            index += STEPS[step]
            scale.append(chromatic[index % 12])
        return scale

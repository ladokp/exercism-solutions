class Garden:
    def __init__(self, diagram, students=None):
        if students is None:
            students = (
                "Alice",
                "Bob",
                "Charlie",
                "David",
                "Eve",
                "Fred",
                "Ginny",
                "Harriet",
                "Ileana",
                "Joseph",
                "Kincaid",
                "Larry",
            )
        self.students = dict(
            (key, value)
            for key, value in zip(
                sorted(students), range(0, 2 * len(students) + 1, 2)
            )
        )
        self.diagram = tuple(
            tuple(
                {
                    "G": "Grass",
                    "C": "Clover",
                    "R": "Radishes",
                    "V": "Violets",
                }[shortcut]
                for shortcut in line
            )
            for line in diagram.splitlines()
        )

    def plants(self, name):
        return [
            self.diagram[0][index := self.students.get(name)],
            self.diagram[0][index + 1],
            self.diagram[1][index],
            self.diagram[1][index + 1],
        ]

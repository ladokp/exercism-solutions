"""
Module for managing a garden with plants assigned to students.
"""

DEFAULT_STUDENTS = (
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
"""
A tuple of default student names used when no specific list is provided.
"""

TRANSLATION_DICTIONARY = {
    "G": "Grass",
    "C": "Clover",
    "R": "Radishes",
    "V": "Violets",
}
"""
A dictionary mapping plant code letters to their respective plant names.
"""


class Garden:
    """
    Represents a garden with plants assigned to students.
    """

    def __init__(self, diagram, students=DEFAULT_STUDENTS):
        """
        Initializes the Garden with a plant diagram and optionally a list of students.

        :param diagram: A string representing the rows of plants.
        :param students: A tuple of student names, defaults to DEFAULT_STUDENTS.
        """
        self.students = dict(
            (key, value)
            for key, value in zip(
                sorted(students), range(0, 2 * len(students) + 1, 2)
            )
        )
        self.diagram = tuple(
            tuple(TRANSLATION_DICTIONARY[shortcut] for shortcut in line)
            for line in diagram.splitlines()
        )

    def plants(self, name):
        """
        Returns the list of plants assigned to the specified student.

        :param name: The name of the student.
        :return: A list of plant names assigned to the student.
        """
        return [
            *self.diagram[0][(index := self.students.get(name)) : index + 2],
            *self.diagram[1][index : index + 2],
        ]

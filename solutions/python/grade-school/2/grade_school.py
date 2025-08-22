"""
Module for managing school student rosters and their grades.
"""

from collections import defaultdict


class School:
    """
    A class to represent a school with student rosters by grade.
    """

    def __init__(self):
        """
        Initialize a new School object.
        """
        self.addition_log = []
        self.added_students = []
        self.grade_log = []
        self.roster_dict = defaultdict(list)

    def add_student(self, name, grade):
        """
        Add a student to the school roster.

        Parameters:
            name (str): The name of the student.
            grade (int): The grade of the student.
        """
        if name not in self.added_students:
            self.added_students.append(name)
            self.roster_dict[grade].append(name)
            self.roster_dict[grade].sort()
            if grade not in self.grade_log:
                self.grade_log.append(grade)
                self.grade_log.sort()
            self.addition_log.append(True)
        else:
            self.addition_log.append(False)

    def roster(self):
        """
        Get the list of all students in the school, sorted by grade and then by name.

        Returns:
            list: A list of student names.
        """
        return [
            element
            for key in self.grade_log
            for element in self.roster_dict[key]
        ]

    def grade(self, grade_number):
        """
        Get the list of students in a specific grade.

        Parameters:
            grade_number (int): The grade number to query.

        Returns:
            list: A list of student names in the specified grade.
        """
        return self.roster_dict[grade_number]

    def added(self):
        """
        Get the log of whether each student addition was successful.

        Returns:
            list: A list of boolean values indicating success (True) or failure (False).
        """
        return self.addition_log

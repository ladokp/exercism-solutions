from collections import defaultdict


class School:
    def __init__(self):
        self.addition_log = []
        self.added_students = []
        self.grade_log = []
        self.roster_dict = defaultdict(list)

    def add_student(self, name, grade):
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
        return [
            element
            for key in self.grade_log
            for element in self.roster_dict[key]
        ]

    def grade(self, grade_number):
        return self.roster_dict[grade_number]

    def added(self):
        return self.addition_log

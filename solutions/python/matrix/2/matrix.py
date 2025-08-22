class Matrix:
    def __init__(self, matrix_string):
        self.matrix = tuple(
            [int(number) for number in line.split()]
            for line in matrix_string.splitlines()
        )

    def row(self, index):
        return self.matrix[index - 1]

    def column(self, index):
        return [row[index - 1] for row in self.matrix]

class Matrix:
    def __init__(self, matrix_string):
        self.matrix = tuple(
            [int(number) for number in line.split(" ")]
            for line in matrix_string.split("\n")
        )
        self.transposed = tuple(zip(*self.matrix))

    def row(self, index):
        return self.matrix[index - 1]

    def column(self, index):
        return list(self.transposed[index - 1])

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y


class WordSearch(object):
    def __init__(self, puzzle):
        self.matrix = [list(row) for row in puzzle]
        self.n = len(self.matrix)
        self.m = len(self.matrix[0])

    def is_ok(self, x, y):
        return all((x >= 0, x < self.m, y >= 0, y < self.n))

    def search(self, word):
        word_length = len(word)
        delta_x = (0, 1, 1, 1, 0, -1, -1, -1)
        delta_y = (-1, -1, 0, 1, 1, 1, 0, -1)
        for index_y in range(self.n):
            for index_x in range(self.m):
                if self.matrix[index_y][index_x] != word[0]:
                    continue
                for direction in zip(delta_x, delta_y):
                    for index in range(1, word_length):
                        new_x = index_x + direction[0] * index
                        new_y = index_y + direction[1] * index
                        if (
                            not self.is_ok(new_x, new_y)
                            or self.matrix[new_y][new_x] != word[index]
                        ):
                            break
                        if index == word_length - 1:
                            return Point(index_x, index_y), Point(new_x, new_y)

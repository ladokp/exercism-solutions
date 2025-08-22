from collections import defaultdict

WHITE = "W"
BLACK = "B"
NONE = " "


class Board:
    def __init__(self, board):
        self.board = board

    def __are_valid__(self, x, y):
        return len(self.board[0]) > x >= 0 and len(self.board) > y >= 0

    def __get_value__(self, x, y):
        return self.board[y][x]

    def territory(self, x, y):
        if not self.__are_valid__(x, y):
            raise ValueError("Invalid coordinate")
        stack = [(x, y)]
        area = set()
        stones = set()
        while stack:
            x, y = stack.pop()
            if (x, y) in area or not self.__are_valid__(x, y):
                continue
            if self.__get_value__(x, y) == NONE:
                area.add((x, y))
                stack += [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
            else:
                stones.add(self.__get_value__(x, y))
        stone = stones.pop() if area and len(stones) == 1 else NONE
        return stone, area

    def territories(self):
        result = defaultdict(set)
        for row_idx, row in enumerate(self.board):
            for col_idx, _ in enumerate(row):
                stone, territory = self.territory(col_idx, row_idx)
                result[stone] |= territory
        return result

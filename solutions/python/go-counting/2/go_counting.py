"""
This module contains the `Board` class for handling operations related to a game board.
"""

from collections import defaultdict

WHITE = "W"
BLACK = "B"
NONE = " "


class Board:
    """
    A class to represent a game board and handle operations related to territories on the board.
    """

    def __init__(self, board):
        """
        Initialize the Board with the given board layout.

        Parameters:
        board (2d list of str): The game board layout.
        """
        self.board = board

    def __are_valid__(self, x, y):
        """
        Check if the given coordinates are valid on the board.

        Parameters:
        x (int): The x-coordinate.
        y (int): The y-coordinate.

        Returns:
        bool: True if the coordinates are valid, False otherwise.
        """
        return len(self.board[0]) > x >= 0 and len(self.board) > y >= 0

    def __get_value__(self, x, y):
        """
        Get the value at the given coordinates on the board.

        Parameters:
        x (int): The x-coordinate.
        y (int): The y-coordinate.

        Returns:
        str: The value at the coordinates on the board.
        """
        return self.board[y][x]

    def territory(self, x, y):
        """
        Determine the owner and the set of coordinates that form the territory starting from (x, y).

        Parameters:
        x (int): The x-coordinate.
        y (int): The y-coordinate.

        Returns:
        tuple: A tuple containing the owner and the set of coordinates in the territory.
        """
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
        """
        Calculate all territories on the board.

        Returns:
        defaultdict: A dictionary with keys as the owners and values as sets of coordinates forming the territories.
        """
        result = defaultdict(set)
        for row_idx, row in enumerate(self.board):
            for col_idx, _ in enumerate(row):
                stone, territory = self.territory(col_idx, row_idx)
                result[stone] |= territory
        return result

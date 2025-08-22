"""This module contains a class for the Connect Game, allowing two players to compete 
on a board and determining the winner based on connected symbols."""


class ConnectGame:
    """A class to represent the Connect Game.

    Attributes:
        player1 (str): The symbol for player 1 (default is "O").
        player2 (str): The symbol for player 2 (default is "X").
        board (tuple): A tuple of tuples representing the game board.
        neighbours (list): A list of tuples representing the possible neighboring positions.
    """

    def __init__(
        self, board: str, player1: str = "O", player2: str = "X"
    ) -> None:
        self.player1 = player1
        self.player2 = player2
        self.board = tuple(
            tuple(stone for stone in row)
            for row in board.replace(" ", "").splitlines()
        )
        self.neighbours = (
            (0, 1),
            (1, 0),
            (1, -1),
            (0, -1),
            (-1, 0),
            (-1, 1),
        )

    def get_winner(self) -> str:
        """Checks for the winner of the game.

        Returns:
            str: The symbol of the winning player or an empty string if there is no winner.
        """
        for player in [self.player1, self.player2]:
            result = self.checker(player)
            if result is True:
                return player
        return ""

    def checker(self, player: str) -> bool:
        """Checks if the specified player has connected their stones.

        Args:
            player (str): The symbol of the player to check for connections.

        Returns:
            bool: True if the player has connected their stones, False otherwise.
        """
        board = self.board
        if player == self.player2:
            board = list(zip(*self.board))
        stones = [
            (index, 0)
            for index, stone in enumerate(board[0])
            if stone == player
        ]
        for stone in stones:
            for neighbour in self.neighbours:
                row = stone[1] + neighbour[1]
                col = stone[0] + neighbour[0]
                if row >= 0 and col >= 0:
                    try:
                        if (
                            board[row][col] == player
                            and (col, row) not in stones
                        ):
                            stones.append((col, row))
                    except IndexError:
                        continue
        ends = (
            (index, len(board) - 1)
            for index, stone in enumerate(board[-1])
            if stone == player
        )
        for end in ends:
            if end in stones:
                return True
        return False

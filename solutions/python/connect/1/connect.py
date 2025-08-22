class ConnectGame:
    def __init__(self, board, player1="O", player2="X"):
        self.player1 = player1
        self.player2 = player2
        self.board = tuple(
            tuple(stone for stone in row)
            for row in board.replace(" ", "").splitlines()
        )
        self.neighbours = [
            (0, 1),
            (1, 0),
            (1, -1),
            (0, -1),
            (-1, 0),
            (-1, 1),
        ]

    def get_winner(self):
        for player in [self.player1, self.player2]:
            result = self.checker(player)
            if result is True:
                return player
        return ""

    def checker(self, player):
        board = self.board
        if player == self.player2:
            board = list(zip(*self.board))
        stones = [
            (index, 0)
            for index, stone in enumerate(board[0])
            if stone == player
        ]
        ends = [
            (index, len(board) - 1)
            for index, stone in enumerate(board[-1])
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
        for end in ends:
            if end in stones:
                return True

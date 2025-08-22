/**
 * "Player O" plays from top to bottom, "Player X" plays from left to right.
 * @param {Array<Array<string>>} board - The game board represented as a 2D array.
 */
export class Board {
  /**
   * Constructs a new Board instance.
   * @param {Array<Array<string>>} board - The game board represented as a 2D array.
   */
  constructor(board) {
    this.board = board.map((b) => [...b]);
  }

  /**
   * Determines the winner of the game.
   * @returns {string} - The player who won ('X' or 'O'), or an empty string if there is no winner.
   */
  winner() {
    const players = ['X', 'O'];
    for (const player of players) {
      if (this.checkWin(player)) {
        return player;
      }
    }
    return '';
  }

  /**
   * Checks if the specified player has won the game.
   * @param {string} player - The player to check ('X' or 'O').
   * @returns {boolean} - True if the player has won, false otherwise.
   */
  checkWin(player) {
    const positions = this.startPositions(player);
    for (const position of positions) {
      if (this.search(position, player, [])) {
        return true;
      }
    }
    return false;
  }

  /**
   * Recursively searches for a winning path for the specified player.
   * @param {Object} pos - The current position.
   * @param {number} pos.x - The x-coordinate of the position.
   * @param {number} pos.y - The y-coordinate of the position.
   * @param {string} XorO - The player to search for ('X' or 'O').
   * @param {Array<Object>} checked - The positions that have already been checked.
   * @returns {boolean} - True if a winning path is found, false otherwise.
   */
  search(pos, XorO, checked) {
    if (!this.matches(pos, XorO)) {
      return false;
    }
    if (this.winningSpot(pos, XorO)) {
      return true;
    }

    checked = checked.slice(0);
    checked.push(pos);
    const matches = this.neighbors(pos).filter(
      ({ x, y }) =>
        this.matches({ x, y }, XorO) &&
        checked.filter((spot) => spot.x === x && spot.y === y).length === 0,
    );
    if (matches.length === 0) {
      return false;
    }

    return matches.some((spot) => this.search(spot, XorO, checked));
  }

  /**
   * Gets the neighboring positions for the specified position.
   * @param {Object} pos - The current position.
   * @param {number} pos.x - The x-coordinate of the position.
   * @param {number} pos.y - The y-coordinate of the position.
   * @returns {Array<Object>} - The neighboring positions.
   */
  neighbors({ x, y }) {
    return [
      { x, y: y + 2 },
      { x, y: y - 2 },

      { x: x + 1, y: y + 1 },
      { x: x - 1, y: y + 1 },

      { x: x + 1, y: y - 1 },
      { x: x - 1, y: y - 1 },
    ];
  }

  /**
   * Gets the starting positions for the specified player.
   * @param {string} XorO - The player to get starting positions for ('X' or 'O').
   * @returns {Array<Object>} - The starting positions.
   */
  startPositions(XorO) {
    return XorO === 'X'
      ? this.board.map((pos, i) => ({ x: i, y: i }))
      : this.board[0].map((pos, i) => ({ x: 0, y: i }));
  }

  /**
   * Checks if the specified position is a winning spot for the player.
   * @param {Object} pos - The current position.
   * @param {number} pos.x - The x-coordinate of the position.
   * @param {number} pos.y - The y-coordinate of the position.
   * @param {string} XorO - The player to check for ('X' or 'O').
   * @returns {boolean} - True if the position is a winning spot, false otherwise.
   */
  winningSpot({ x, y }, XorO) {
    return XorO === 'X'
      ? y === this.board[0].length - 1 + x
      : x === this.board.length - 1;
  }

  /**
   * Checks if the specified position matches the player's marker.
   * @param {Object} pos - The current position.
   * @param {number} pos.x - The x-coordinate of the position.
   * @param {number} pos.y - The y-coordinate of the position.
   * @param {string} XorO - The player to check for ('X' or 'O').
   * @returns {boolean} - True if the position matches the player's marker, false otherwise.
   */
  matches({ x, y }, XorO) {
    return this.board[x] !== undefined && this.board[x][y] === XorO;
  }
}

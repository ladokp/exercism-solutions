const MINE = '*';

const DELTAS = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [1, 1],
  [1, 0],
  [1, -1],
  [0, 1],
  [0, -1],
];

/**
 * Gets the adjacent squares if it is on the board.
 * 
 * @param {string[][]} board - The minesweeper board.
 * @param {number} x - The x-coordinate of the current cell.
 * @param {number[]} d - The delta to the adjacent cell.
 * @returns {string[]} True if the adjacent square is on the board.
 */
function getAdjacentSquares(board, x, d) {
  return board[x + d[0]];
}

/**
 * Checks if the adjacent square has a mine.
 * 
 * @param {string[][]} board - The minesweeper board.
 * @param {number} x - The x-coordinate of the current cell.
 * @param {number} y - The y-coordinate of the current cell.
 * @param {number[]} d - The delta to the adjacent cell.
 * @returns {boolean} True if the adjacent square has a mine.
 */
function adjacentSquareHasMine(board, x, y, d) {
  const adjacentSquares = getAdjacentSquares(board, x, d);
  return adjacentSquares[y + d[1]] === MINE;
}

/**
 * Counts the number of mines adjacent to a given cell.
 * 
 * @param {string[][]} board - The minesweeper board.
 * @param {number} x - The x-coordinate of the current cell.
 * @param {number} y - The y-coordinate of the current cell.
 * @returns {number} The number of adjacent mines.
 */
function countAdjacentMines(board, x, y) {
  return DELTAS.filter((d) => getAdjacentSquares(board, x, d)).filter(
    (d) => adjacentSquareHasMine(board, x, y, d),
  ).length;
}

/**
 * Converts a cell to a mine or the count of adjacent mines.
 * 
 * @param {string} cell - The value of the current cell.
 * @param {string[][]} inputBoard - The minesweeper board.
 * @param {number} x - The x-coordinate of the current cell.
 * @param {number} y - The y-coordinate of the current cell.
 * @returns {string} The cell value as a mine or count of adjacent mines.
 */
function cellToMineOrCount(cell, inputBoard, x, y) {
  if (cell === MINE) return MINE;
  return countAdjacentMines(inputBoard, x, y) || ' ';
}

/**
 * Converts the board to a string format.
 * 
 * @param {string[][]} board - The minesweeper board.
 * @returns {string[]} The board in string format.
 */
function stringify(board) {
  return board.map((row) => row.join(''));
}

/**
 * Checks if no data is present in the rows.
 * 
 * @param {string[]} rows - The rows of the minesweeper board.
 * @returns {boolean} True if no data is present.
 */
function noDataPresent(rows) {
  return rows.length === 0 || rows[0].length === 0;
}

/**
 * Annotates the minesweeper board with the count of adjacent mines.
 * 
 * @param {string[]} rows - The rows of the minesweeper board.
 * @returns {string[]} The annotated minesweeper board.
 */
export function annotate(rows) {
  if (noDataPresent(rows)) return rows;
  const inputBoard = rows.map((row) => [...row]);
  return stringify(
    inputBoard.map((row, x) =>
      [...row].map((cell, y) => cellToMineOrCount(cell, inputBoard, x, y)),
    ),
  );
}

/**
 * Searches for a word horizontally in the grid.
 * @param {Object} params - The parameters.
 * @param {string} params.word - The word to search for.
 * @param {string[]} params.grid - The grid to search within.
 * @returns {Object|boolean} - The start and end coordinates if found, otherwise false.
 */
function searchHorizontally({ word, grid }) {
  let rowIndex = 0;
  let startCol;
  let start;
  let end;

  const getCoords = () => [
    [rowIndex + 1, startCol],
    [rowIndex + 1, startCol + word.length - 1],
  ];

  const getStartCol = (currentWord) => 1 + grid[rowIndex].indexOf(currentWord);

  while (rowIndex < grid.length) {
    startCol = getStartCol(word);
    if (startCol) {
      [start, end] = getCoords();
    } else {
      startCol = getStartCol([...word].reverse().join(''));
      if (startCol) {
        [end, start] = getCoords();
      }
    }
    if (start && end) {
      return { start, end };
    }
    rowIndex += 1;
  }
  return false;
}

/**
 * Flips the coordinates (reverses the order of start and end).
 * @param {Object} coords - The coordinates to flip.
 * @returns {Object|undefined} - The flipped coordinates if valid, otherwise undefined.
 */
function flipCoordinates(coords) {
  if (!coords) {
    return undefined;
  }
  return {
    start: coords.start.reverse(),
    end: coords.end.reverse(),
  };
}

/**
 * Flips the grid (transposes rows and columns).
 * @param {string[]} grid - The grid to flip.
 * @returns {string[]} - The flipped grid.
 */
function flipGrid(grid) {
  return [...grid[0]]
    .map((_, c) => grid.map((row) => row[c]))
    .map((row) => row.join(''));
}

/**
 * Searches for a word diagonally in the grid.
 * @param {number} r - The starting row index.
 * @param {number} c - The starting column index.
 * @param {string} word - The word to search for.
 * @param {string[]} grid - The grid to search within.
 * @param {number} rIncrement - The row increment direction.
 * @param {Function} outOfRange - The function to check if the coordinates are out of range.
 * @param {Function} buildCoords - The function to build the coordinates.
 * @returns {Object|undefined} - The coordinates if found, otherwise undefined.
 */
function diagonalFind(r, c, word, grid, rIncrement, outOfRange, buildCoords) {
  let currentRow = r;
  let currentColumn = c;
  let foundLetters = '';
  const startR = r + 1;
  const startC = c + 1;
  let result;

  word.split('').forEach((letter) => {
    if (
      !outOfRange(
        currentRow,
        currentColumn,
        word.length,
        grid[currentRow].length,
        foundLetters.length,
      )
    ) {
      const currLetterInGrid = grid[currentRow].charAt(currentColumn);
      currentColumn += 1;
      if (currLetterInGrid === letter) {
        foundLetters += currLetterInGrid;
        if (foundLetters === word) {
          result = buildCoords(startR, startC, currentRow, currentColumn);
        }
        currentRow += rIncrement;
      }
    }
  });
  return result;
}

/**
 * Searches for a word diagonally from top-left to bottom-right.
 * @param {number} r - The starting row index.
 * @param {number} c - The starting column index.
 * @param {string} word - The word to search for.
 * @param {string[]} grid - The grid to search within.
 * @returns {Object|undefined} - The coordinates if found, otherwise undefined.
 */
function findAWordDiagonallyTopDown(r, c, word, grid) {
  function outOfRange(row, column, words, columns, letters) {
    return (
      row > columns - words + letters || column > columns - words + letters
    );
  }

  function buildCoords(startR, startC, row, column) {
    return {
      start: [startR, startC],
      end: [row + 1, column],
    };
  }

  return diagonalFind(r, c, word, grid, 1, outOfRange, buildCoords);
}

/**
 * Searches for a word diagonally from bottom-left to top-right.
 * @param {number} r - The starting row index.
 * @param {number} c - The starting column index.
 * @param {string} word - The word to search for.
 * @param {string[]} grid - The grid to search within.
 * @returns {Object|undefined} - The coordinates if found, otherwise undefined.
 */
function findAWordDiagonallyBottomUp(r, c, word, grid) {
  function outOfRange(row, column, words, columns, letters) {
    return row < words - letters - 1 || column > columns - words + letters;
  }

  function buildCoords(startR, startC, row, column) {
    return {
      start: [startR, startC],
      end: [row + 1, column],
    };
  }

  return diagonalFind(r, c, word, grid, -1, outOfRange, buildCoords);
}

/**
 * Formats the coordinates based on whether the word is reversed.
 * @param {Object} coords - The coordinates to format.
 * @param {boolean} isReversed - Whether the word is reversed.
 * @returns {Object} - The formatted coordinates.
 */
function formatCoordinates(coords, isReversed) {
  return isReversed
    ? { start: coords.end, end: coords.start }
    : coords;
}

/**
 * Searches for a word diagonally in the grid.
 * @param {Object} params - The parameters.
 * @param {string} params.word - The word to search for.
 * @param {string[]} params.grid - The grid to search within.
 * @param {boolean} [params.isReversed=false] - Whether to search for the reversed word.
 * @param {boolean} [params.fromTop=true] - Whether to search from top to bottom.
 * @returns {Object|undefined} - The coordinates if found, otherwise undefined.
 */
function searchDiagonally({ word, grid, isReversed = false, fromTop = true }) {
  const rIncrement = fromTop ? 1 : -1;
  const startRow = fromTop ? 0 : grid.length - 1;
  const endRow = fromTop ? (r) => r < grid.length : (r) => r >= 0;
  const findDirection = fromTop
    ? findAWordDiagonallyTopDown
    : findAWordDiagonallyBottomUp;

  for (let r = startRow; endRow(r); r += rIncrement) {
    for (let c = 0; c < grid[r].length; c += 1) {
      const possibleCoords = findDirection(r, c, word, grid);
      if (possibleCoords) {
        return formatCoordinates(possibleCoords, isReversed);
      }
    }
  }

  if (!isReversed) {
    // now find the reversed version
    const reversedWord = [...word].reverse().join('');
    return searchDiagonally({
      word: reversedWord,
      grid,
      isReversed: true,
      fromTop,
    });
  }
  return undefined;
}

/**
 * Searches for a word in any direction within the grid.
 * @param {string} word - The word to search for.
 * @param {string[]} grid - The grid to search within.
 * @returns {Object|boolean} - The start and end coordinates if found, otherwise false.
 */
function findWordInAnyDirection(word, grid) {
  return (
    searchHorizontally({ word, grid }) ||
    flipCoordinates(searchHorizontally({ word, grid: flipGrid(grid) })) ||
    searchDiagonally({ word, grid }) ||
    searchDiagonally({ word, grid, fromTop: false })
  );
}

class WordSearch {
  /**
   * Creates an instance of WordSearch.
   * @param {string[]} grid - The grid to search within.
   */
  constructor(grid) {
    this.grid = grid;
  }

  /**
   * Finds coordinates of multiple words in the grid.
   * @param {string[]} words - The words to find.
   * @returns {Object} - An object with words as keys and their coordinates as values.
   */
  find(words) {
    return words
      .map((word) => ({ [word]: findWordInAnyDirection(word, this.grid) }))
      .reduce((acc, oneWord) => Object.assign(acc, oneWord), {});
  }
}

module.exports = WordSearch;

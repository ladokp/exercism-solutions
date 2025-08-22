/**
 * Generates a spiral matrix of a given size.
 *
 * @param {number} size - The size of the spiral matrix.
 * @returns {number[][]} The generated spiral matrix.
 */
export const spiralMatrix = (size) => {
  // Initialize an empty matrix with the given size
  const spiral = Array.from({ length: size }, () => Array(size).fill(0));

  const totalNumbers = size ** 2;
  let currentNumber = 1;
  let topLeft = 0;
  let bottomRight = size - 1;

  // Fill the matrix in a spiral order
  while (currentNumber <= totalNumbers) {
    // Fill the top row
    for (let x = topLeft; x <= bottomRight; x++) {
      spiral[topLeft][x] = currentNumber;
      currentNumber++;
    }

    // Fill the right column
    for (let y = topLeft + 1; y <= bottomRight; y++) {
      spiral[y][bottomRight] = currentNumber;
      currentNumber++;
    }

    // Fill the bottom row
    for (let x = bottomRight - 1; x >= topLeft; x--) {
      spiral[bottomRight][x] = currentNumber;
      currentNumber++;
    }

    // Fill the left column
    for (let y = bottomRight - 1; y > topLeft; y--) {
      spiral[y][topLeft] = currentNumber;
      currentNumber++;
    }

    // Move to the next inner layer
    topLeft++;
    bottomRight--;
  }

  return spiral;
};

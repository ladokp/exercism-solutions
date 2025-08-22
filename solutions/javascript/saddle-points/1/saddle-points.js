/**
 * Finds the saddle points in a matrix.
 * A saddle point is an element of the matrix that is the minimum in its column and the maximum in its row.
 *
 * @param {number[][]} matrix - The input matrix.
 * @returns {Object[]} - An array of objects representing the saddle points with their row and column indices.
 */
function saddlePoints(matrix) {
  const maximumRowValues = getMaximumRowValues(matrix);
  const minimumColumnValues = getMinimumColumnValues(matrix);
  return findSaddlePoints(maximumRowValues, minimumColumnValues);
}

/**
 * Gets the maximum values of each row in the matrix.
 *
 * @param {number[][]} matrix - The input matrix.
 * @returns {number[]} - An array of maximum values for each row.
 */
function getMaximumRowValues(matrix) {
  return matrix.map(row => Math.max(...row));
}

/**
 * Gets the minimum values of each column in the matrix.
 *
 * @param {number[][]} matrix - The input matrix.
 * @returns {number[]} - An array of minimum values for each column.
 */
function getMinimumColumnValues(matrix) {
  const minimumColumnValues = [];
  for (let i = 0; i < matrix[0].length; i++) {
    let minimumColumnValue = Number.MAX_VALUE;
    for (let j = 0; j < matrix.length; j++) {
      minimumColumnValue = Math.min(minimumColumnValue, matrix[j][i]);
    }
    minimumColumnValues.push(minimumColumnValue);
  }
  return minimumColumnValues;
}

/**
 * Finds the saddle points given the maximum row values and minimum column values.
 *
 * @param {number[]} maximumRowValues - The maximum values of each row.
 * @param {number[]} minimumColumnValues - The minimum values of each column.
 * @returns {Object[]} - An array of objects representing the saddle points with their row and column indices.
 */
function findSaddlePoints(maximumRowValues, minimumColumnValues) {
  const resultPoints = [];
  for (let i = 0; i < maximumRowValues.length; i++) {
    for (let j = 0; j < minimumColumnValues.length; j++) {
      if (maximumRowValues[i] === minimumColumnValues[j]) {
        resultPoints.push({ row: i + 1, column: j + 1 });
      }
    }
  }
  return resultPoints;
}

export { saddlePoints };
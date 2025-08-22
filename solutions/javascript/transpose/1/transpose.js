/**
 * Trims trailing undefined values from an array.
 *
 * @param {Array} array - The array to trim.
 * @returns {Array} - The trimmed array.
 */
function trimTrailingUndefined(array) {
  const trailingUndefinedCount = array.slice().reverse().findIndex((x) => x !== undefined);
  return array.slice(0, array.length - trailingUndefinedCount);
}

/**
 * Transposes a 2D array of characters, filling in missing characters with spaces.
 *
 * @param {string[]} input - The array of strings to transpose.
 * @returns {string[]} - The transposed array of strings.
 */
export function transpose(input) {
  const maxCol = Math.max(0, ...input.map((row) => row.length));
  return Array.from({ length: maxCol }, (_, col) => 
    trimTrailingUndefined(input.map((row) => row[col]))
      .map((charOrUndefined) => charOrUndefined || ' ')
      .join('')
  );
}
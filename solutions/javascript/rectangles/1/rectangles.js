/**
 * Counts the number of rectangles in a given diagram.
 * @param {string[]} diagram - Array of strings representing the diagram.
 * @returns {number} The number of rectangles found in the diagram.
 */
export function count(diagram) {
  const rows = diagram.length;
  const cols = rows ? diagram[0].length : 0;

  let rectangles = 0;

  // Iterate through all possible top-left corners of rectangles
  for (let y = 0; y < rows - 1; y += 1) {
    for (let x = 0; x < cols - 1; x += 1) {
      if (diagram[y].charAt(x) === '+') {
        // Iterate through all possible bottom-right corners of rectangles
        for (let j = y + 1; j < rows; j += 1) {
          for (let i = x + 1; i < cols; i += 1) {
            // Check if all four corners are valid
            if (
              diagram[j].charAt(i) === '+' &&
              diagram[y].charAt(i) === '+' &&
              diagram[j].charAt(x) === '+'
            ) {
              let validRectangle = true;

              // Check if all horizontal sides are valid
              for (let s = x + 1; s < i; s += 1) {
                if (!'+-'.includes(diagram[y].charAt(s))) validRectangle = false;
                if (!'+-'.includes(diagram[j].charAt(s))) validRectangle = false;
              }
              // Check if all vertical sides are valid
              for (let t = y + 1; t < j; t += 1) {
                if (!'+|'.includes(diagram[t].charAt(x))) validRectangle = false;
                if (!'+|'.includes(diagram[t].charAt(i))) validRectangle = false;
              }

              // Increment rectangle count if valid
              if (validRectangle) rectangles += 1;
            }
          }
        }
      }
    }
  }
  return rectangles;
}

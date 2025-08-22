/**
 * Generates the rows for a diamond shape based on the input character.
 * @param {string} input - A single uppercase letter (A-Z).
 * @returns {string[]} An array of strings where each string represents a row of the diamond.
 */
export const rows = (input) => {
  const inputIndex = input.charCodeAt() - 65;
  let output = [];
  let i = 0;
  for (i = 0; i <= inputIndex; i++) {
    output.push(getLine(inputIndex, i));
  }
  for (i = inputIndex - 1; i >= 0; i--) {
    output.push(getLine(inputIndex, i));
  }
  return output;
};

/**
 * Generates a single line of the diamond.
 * @param {number} inputIndex - The index of the input character (0 for 'A', 1 for 'B', etc.).
 * @param {number} index - The current line index.
 * @returns {string} The generated line of the diamond.
 */
function getLine(inputIndex, index) {
  const difference = inputIndex - index;
  return `${
    spaceTimes(difference) + printAlphabets(index) + spaceTimes(difference)
  }`;
}

/**
 * Generates the alphabet characters for a specific line of the diamond.
 * @param {number} index - The current line index.
 * @returns {string} The alphabet characters for the line.
 */
function printAlphabets(index) {
  const character = 65 + index;
  if (index === 0) {
    return 'A';
  }
  return (
    String.fromCharCode(character) +
    spaceTimes((index - 1) * 2 + 1) +
    String.fromCharCode(character)
  );
}

/**
 * Generates a string of spaces.
 * @param {number} times - The number of spaces to generate.
 * @returns {string} A string containing the specified number of spaces.
 */
function spaceTimes(times) {
  return ' '.repeat(times);
}

/**
 * OCR Patterns for each digit (0-9), represented as arrays of strings.
 */
const PATTERNS = {
  0: [' _ ', '| |', '|_|', '   '],
  1: ['   ', '  |', '  |', '   '],
  2: [' _ ', ' _|', '|_ ', '   '],
  3: [' _ ', ' _|', ' _|', '   '],
  4: ['   ', '|_|', '  |', '   '],
  5: [' _ ', '|_ ', ' _|', '   '],
  6: [' _ ', '|_ ', '|_|', '   '],
  7: [' _ ', '  |', '  |', '   '],
  8: [' _ ', '|_|', '|_|', '   '],
  9: [' _ ', '|_|', ' _|', '   '],
};

/**
 * Splits the text into rows with each row containing 4 lines.
 * @param {string} text - The input text representing OCR digits.
 * @returns {string[]} - An array of rows.
 */
const splitIntoRows = (text) => {
  const rows = [];
  const lines = text.split('\n');
  for (let rowNumber = 0; rowNumber < lines.length; rowNumber += 4) {
    let row = '';
    for (let rowLine = 0; rowLine < 4; rowLine += 1) {
      row += `${lines[rowNumber + rowLine]}\n`;
    }
    rows.push(row.slice(0, -1));
  }
  return rows;
};

/**
 * Splits a row into individual digits.
 * @param {string} row - A row of OCR digits.
 * @returns {string[]} - An array of digit strings.
 */
const splitIntoDigits = (row) => {
  const digits = [];
  const rows = row.split('\n');
  for (let digitNumber = 0; digitNumber < rows[0].length; digitNumber += 3) {
    let digit = '';
    for (let rowNumber = 0; rowNumber < rows.length; rowNumber += 1) {
      digit += rows[rowNumber].substring(digitNumber, digitNumber + 3);
    }
    digits.push(digit);
  }
  return digits;
};

/**
 * Converts a digit string to its corresponding number.
 * @param {string} text - The digit string to convert.
 * @returns {number|string} - The corresponding number or '?' if invalid.
 */
const getDigit = (text) => {
  const digit = Object.values(PATTERNS)
    .map((x) => x.join(''))
    .indexOf(text);
  return digit === -1 ? '?' : digit;
};

/**
 * Converts a row of OCR digits to a string of numbers.
 * @param {string} row - A row of OCR digits.
 * @returns {string} - A string of numbers.
 */
const valuesInRow = (row) => splitIntoDigits(row).map(getDigit).join('');

/**
 * Converts OCR text to a string of numbers, with rows separated by commas.
 * @param {string} text - The input text representing OCR digits.
 * @returns {string} - The converted string of numbers.
 */
export const convert = (text) => splitIntoRows(text).map(valuesInRow).join(',');

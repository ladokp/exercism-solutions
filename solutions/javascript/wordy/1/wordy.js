// Mapping of supported operations to their corresponding symbols
const OPS = {
  'What is': '',
  'plus': '+',
  'minus': '-',
  'multiplied by': '*',
  'divided by': '/',
  '?': ''
};

/**
 * Helper function to check if a word is not a number.
 * @param {string} word - The word to check.
 * @returns {boolean} - Returns true if the word is not a number.
 */
const notNumber = (word) => isNaN(Number(word));

/**
 * Helper function to check if a word is not a valid operator (+, -, *, /).
 * @param {string} word - The word to check.
 * @returns {boolean} - Returns true if the word is not an operator.
 */
const notOp = (word) => '+-*/'.indexOf(word) === -1;

/**
 * Helper function to check if the index is odd.
 * @param {any} _ - Unused parameter.
 * @param {number} i - The index of the element.
 * @returns {boolean} - Returns true if the index is odd.
 */
const odd = (_, i) => i % 2 === 1;

/**
 * Parses and computes the result of a math expression in question format.
 * @param {string} question - The question string (e.g., 'What is 5 plus 3?').
 * @returns {number} - The computed result of the question.
 * @throws {Error} - Throws an error for unknown operations or syntax errors.
 */
export function answer(question) {
  // Replace words with their corresponding operators and clean up the string
  let words = question
    .replace(/What is|plus|minus|multiplied by|divided by|\?/g, (w) => OPS[w])
    .split(' ')
    .filter((word) => word); // Remove any empty strings

  // Validate the input for unknown operations or syntax errors
  switch (true) {
    case words.filter(notNumber).some(notOp):
      throw new Error('Unknown operation');
    case words.length % 2 === 0 || !words.filter(odd).every(notNumber):
      throw new Error('Syntax error');
  }

  // Perform calculations iteratively until only one result remains
  while (words.length > 1) {
    words = [calculate(...words.slice(0, 3)), ...words.slice(3)];
  }

  return Number(words[0]);
}

/**
 * Performs a calculation based on two operands and an operator.
 * @param {string} a - The first operand.
 * @param {string} op - The operator ('+', '-', '*', '/').
 * @param {string} b - The second operand.
 * @returns {number} - The result of the computation.
 */
function calculate(a, op, b) {
  a = Number(a);
  b = Number(b);

  switch (op) {
    case '+':
      return a + b;
    case '-':
      return a - b;
    case '*':
      return a * b;
    case '/':
      if (b === 0) throw new Error('Division by zero');
      return a / b;
    default:
      throw new Error(`Unknown operator: ${op}`);
  }
}

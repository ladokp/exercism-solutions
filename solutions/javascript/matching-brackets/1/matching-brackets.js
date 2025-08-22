/**
 * Checks if all brackets in the input string are properly paired.
 *
 * This function removes all non-bracket characters from the input string,
 * then verifies that every opening bracket has a corresponding closing bracket
 * and that they are properly nested.
 *
 * @param {string} input - The input string to check for paired brackets.
 * @returns {boolean} - Returns true if all brackets are correctly paired, otherwise false.
 */
export const isPaired = (input) => {
  const brackets = input.replace(/[^{(\[\])}]/g, ''); //eslint-disable-line

  const bracketsAreMatching = (leftBracket, rightBracket) =>
    (leftBracket === '(' && rightBracket === ')') ||
    (leftBracket === '[' && rightBracket === ']') ||
    (leftBracket === '{' && rightBracket === '}');

  const stack = [];

  for (const char of brackets) {
    if (stack.length && bracketsAreMatching(stack[stack.length - 1], char)) {
      stack.pop();
    } else {
      stack.push(char);
    }
  }

  return stack.length === 0;
};

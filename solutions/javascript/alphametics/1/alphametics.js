/**
 * Array of digits from 0 to 9.
 * @constant {number[]}
 */
const DIGITS = [...Array(10).keys()];

/**
 * Solves the given alphametics puzzle.
 * @param {string} puzzle - The puzzle to solve.
 * @returns {Object|null} - The solution as a mapping of letters to digits, or null if no solution is found.
 */
export function solve(puzzle) {
  const parts = puzzle
    .split(/[+|==]/g)
    .map((part) => part.trim())
    .filter((part) => part !== '');

  if (parts.length < 3) {
    return null;
  }

  const firstLetters = new Set(parts.map((part) => part[0]));
  const letterCounts = countLetters(parts, parts.pop());

  return tryPermutations(letterCounts, firstLetters);
}

/**
 * Counts the occurrences of each letter in the terms and the total.
 * @param {string[]} terms - The terms in the puzzle.
 * @param {string} total - The total in the puzzle.
 * @returns {Object} - The counts of each letter.
 */
function countLetters(terms, total) {
  const counts = {};

  terms.forEach((term) => {
    [...term].forEach((letter, i, { length }) => {
      counts[letter] = (counts[letter] ?? 0) + 10 ** (length - 1 - i);
    });
  });

  [...total].forEach((letter, i, { length }) => {
    counts[letter] = (counts[letter] ?? 0) - 10 ** (length - 1 - i);
  });

  return counts;
}

/**
 * Tests a permutation of numbers to see if it solves the puzzle.
 * @param {Object} letterCounts - The counts of each letter.
 * @param {number[]} numbers - The permutation of numbers to test.
 * @returns {Object|null} - The solution as a mapping of letters to digits, or null if the permutation does not solve the puzzle.
 */
function testPermutation(letterCounts, numbers) {
  const letters = Object.keys(letterCounts);
  const counts = Object.values(letterCounts);
  return counts.reduce((sum, count, i) => sum + count * numbers[i], 0) === 0
    ? letters.reduce(
        (solution, letter, i) => ({ ...solution, [letter]: numbers[i] }),
        {},
      )
    : null;
}

/**
 * Tries all permutations of numbers to find a solution to the puzzle.
 * @param {Object} letterCounts - The counts of each letter.
 * @param {Set} firstLetters - The set of first letters in the terms.
 * @param {number[]} [numbers=[]] - The current permutation of numbers.
 * @returns {Object|null} - The solution as a mapping of letters to digits, or null if no solution is found.
 */
function tryPermutations(letterCounts, firstLetters, numbers = []) {
  const letters = Object.keys(letterCounts);
  if (numbers.length === letters.length) {
    return testPermutation(letterCounts, numbers);
  }

  for (const digit of DIGITS) {
    if (
      numbers.includes(digit) ||
      (digit === 0 && firstLetters.has(letters[numbers.length]))
    ) {
      continue;
    }

    const result = tryPermutations(letterCounts, firstLetters, [
      ...numbers,
      digit,
    ]);

    if (result) {
      return result;
    }
  }

  return null;
}

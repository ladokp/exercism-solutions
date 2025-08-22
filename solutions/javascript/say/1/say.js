/**
 * @typedef {Object} NumberObj
 * @property {number} current - The current number being processed.
 */

/**
 * A dictionary of small numbers up to 19.
 * @type {Object<number, string>}
 */
const smallNumbers = {
  0: 'zero',
  1: 'one',
  2: 'two',
  3: 'three',
  4: 'four',
  5: 'five',
  6: 'six',
  7: 'seven',
  8: 'eight',
  9: 'nine',
  10: 'ten',
  11: 'eleven',
  12: 'twelve',
  13: 'thirteen',
  14: 'fourteen',
  15: 'fifteen',
  16: 'sixteen',
  17: 'seventeen',
  18: 'eighteen',
  19: 'nineteen',
};

/**
 * A dictionary of decades from 20 to 90.
 * @type {Object<number, string>}
 */
const decades = {
  20: 'twenty',
  30: 'thirty',
  40: 'forty',
  50: 'fifty',
  60: 'sixty',
  70: 'seventy',
  80: 'eighty',
  90: 'ninety',
};

/**
 * A dictionary of big numbers like thousand, million, and billion.
 * @type {Object<number, string>}
 */
const bigNumbers = {
  1000: 'thousand',
  1000000: 'million',
  1000000000: 'billion',
};

/**
 * Convert the big part of a number to words.
 * @param {NumberObj} num - The number object containing the current number being processed.
 * @returns {string} - The words representing the big part of the number.
 */
const bigPart = (num) => {
  let res = '';
  for (let bigNumber = 1000000000; bigNumber >= 1000; bigNumber /= 1000) {
    if (num.current >= bigNumber) {
      const factor = Math.floor(num.current / bigNumber);
      res += `${threeDigit(factor)} ${bigNumbers[bigNumber]} `;
      num.current -= factor * bigNumber;
    }
  }
  return res;
};

/**
 * Convert a two-digit number to words.
 * @param {number} num - The number to convert.
 * @returns {string} - The words representing the two-digit number.
 */
const twoDigit = (num) => {
  return num < 20 ? smallNumbers[num] : sayDecade(num);
};

/**
 * Convert a three-digit number to words.
 * @param {number} num - The number to convert.
 * @returns {string} - The words representing the three-digit number.
 */
const threeDigit = (num) => {
  return num < 100
    ? twoDigit(num)
    : `${smallNumbers[Math.floor(num / 100)]} hundred ${twoDigit(num % 100)}`;
};

/**
 * Convert a decade number to words.
 * @param {number} num - The number to convert.
 * @returns {string} - The words representing the decade number.
 */
const sayDecade = (num) => {
  for (let decade = 90; decade >= 20; decade -= 10) {
    if (num >= decade) {
      return num % 10 === 0 ? decades[decade] : `${decades[decade]}-${smallNumbers[num - decade]}`;
    }
  }
  return '';
};

/**
 * Convert a number to words.
 * @param {number} n - The number to convert.
 * @returns {string} - The words representing the number.
 * @throws {Error} - Throws an error if the number is not between 0 and 999,999,999,999.
 */
export const say = (n) => {
  if (n < 0 || n >= 1000000000000) {
    throw new Error('Number must be between 0 and 999,999,999,999.');
  }
  const number = { current: n };
  let result = bigPart(number);
  result += threeDigit(number.current);
  return result.replace(/.zero$/, '').trim();
};

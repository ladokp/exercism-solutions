/**
 * Checks if the provided base is valid.
 * @param {number} base - The base to check.
 * @returns {boolean} - True if the base is invalid, false otherwise.
 */
const isValidBase = (base) => !base || base < 2 || Math.floor(base) !== base;

/**
 * Checks if the input array and base are valid.
 * @param {number[]} array - The array to check.
 * @param {number} base - The base to check against.
 * @returns {boolean} - True if the input is valid, false otherwise.
 */
const isInputValid = (array, base) => {
  if (!array || !array.length) {
    return false;
  }
  const maxVal = base - 1;
  return array.every(value => value >= 0 && value <= maxVal);
};

/**
 * Converts a decimal number to a specified base.
 * @param {number} num - The decimal number to convert.
 * @param {number} outputBase - The base to convert to.
 * @returns {number[]} - The number in the new base as an array of digits.
 */
const convertFromDecimalToBase = (num, outputBase) => {
  if (num === 0) return [0];
  let tmp = num;
  const result = [];
  while (tmp) {
    result.unshift(tmp % outputBase);
    tmp = Math.floor(tmp / outputBase);
  }
  return result;
};

/**
 * Converts a number from one base to another.
 * @param {number[]} array - The number to convert as an array of digits.
 * @param {number} inputBase - The base of the input number.
 * @param {number} outputBase - The base to convert to.
 * @throws Will throw an error if the input or output base is invalid.
 * @throws Will throw an error if the input array is not valid for the input base.
 * @returns {number[]} - The number in the output base as an array of digits.
 */
export const convert = (array, inputBase, outputBase) => {
  if (isValidBase(inputBase)) {
    throw new Error('Wrong input base');
  }
  if (isValidBase(outputBase)) {
    throw new Error('Wrong output base');
  }
  const regexp = new RegExp('^0.', 'g');
  if (array.join('').match(regexp) || !isInputValid(array, inputBase)) {
    throw new Error('Input has wrong format');
  }
  const decimalValue = array.reduce((accumulator, value) => accumulator * inputBase + value, 0);
  return convertFromDecimalToBase(decimalValue, outputBase);
};

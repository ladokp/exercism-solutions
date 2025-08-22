/**
 * Calculates the largest product for a contiguous substring of digits of specified length.
 *
 * @param {string} digits - The string of digits to search.
 * @param {number} seriesLength - The length of the substring for which to calculate the product.
 * @returns {number} - The largest product found.
 * @throws {Error} - If the series length is negative, greater than the length of the digits, or if the digits string contains non-digit characters.
 */
export const largestProduct = (digits, seriesLength) => {
  if (seriesLength === 0) {
    return 1;
  }

  if (seriesLength > digits.length) {
    throw new Error('span must be smaller than string length');
  }

  if (seriesLength < 0) {
    throw new Error('span must not be negative');
  }

  if (!/^\d+$/g.test(digits)) {
    throw new Error('digits input must only contain digits');
  }

  let result = 0;

  for (let i = 0; i <= digits.length - seriesLength; i++) {
    const product = digits
      .substring(i, i + seriesLength)
      .split('')
      .map(Number)
      .reduce((a, b) => a * b, 1); // Provide initial value for reduce

    if (product > result) {
      result = product;
    }
  }

  return result;
};

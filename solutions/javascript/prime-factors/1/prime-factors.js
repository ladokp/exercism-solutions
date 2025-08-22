/**
 * Function to compute the prime factors of a given number.
 * @param {number} n - The number to factorize.
 * @returns {number[]} An array of prime factors.
 * @example
 * primeFactors(28); // returns [2, 2, 7]
 */
export const primeFactors = (n) => {
  if (n < 2) return []; // No prime factors for numbers less than 2

  let num = n;
  const factors = [];
  let currentFactor = 2;

  while (num !== 1) {
    if (num % currentFactor === 0) {
      factors.push(currentFactor);
      num /= currentFactor;
    } else {
      currentFactor += 1;
    }
  }

  return factors;
};

/**
 * Classify a given number as perfect, abundant, or deficient.
 * @param {number} number - The number to be classified. Must be a natural number.
 * @throws Will throw an error if the number is less than 1.
 * @returns {string} The classification of the number ('perfect', 'abundant', or 'deficient').
 */
export function classify(number) {
  if (number < 1) {
    throw new Error('Classification is only possible for natural numbers.');
  }

  const sumOfFactors = Array.from({ length: Math.floor(number / 2) }, (_, i) => i + 1)
    .filter(i => number % i === 0)
    .reduce((acc, factor) => acc + factor, 0);

  if (sumOfFactors === number) return 'perfect';
  if (sumOfFactors < number) return 'deficient';
  return 'abundant';
};

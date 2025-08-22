/**
 * Check if a number is a prime number.
 * 
 * @param {number} number - The number to check for primality.
 * @returns {boolean} - Returns true if the number is prime, otherwise false.
 */
const isPrime = (number) => {
  if (number <= 1) {
    return false;
  }
  if (number === 2 || number === 3) {
    return true;
  }
  if (number % 2 === 0) {
    return false;
  }
  for (let i = 3; i <= Math.sqrt(number); i += 2) {
    if (number % i === 0) {
      return false;
    }
  }
  return true;
};

/**
 * Find the nth prime number.
 * 
 * @param {number} ordinal - The ordinal position of the prime number to find.
 * @throws {Error} - Throws an error if the ordinal is 0.
 * @returns {number} - The nth prime number.
 */
export const prime = (ordinal) => {
  if (ordinal <= 0) {
    throw new Error('there is no zeroth prime');
  }
  if (ordinal === 1) {
    return 2;
  }
  let primeCount = 1;
  let index = 3;
  while (primeCount < ordinal) {
    if (isPrime(index)) {
      primeCount++;
    }
    index += 2;
  }
  return index - 2;
};

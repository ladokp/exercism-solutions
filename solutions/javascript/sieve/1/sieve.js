/**
 * Creates an array containing a range of integers from `first` to `last` inclusive.
 * @param {number} first - The starting integer.
 * @param {number} last - The ending integer.
 * @returns {number[]} An array of integers from `first` to `last`.
 */
function newArrayWithRange(first, last) {
  let i;
  const array = [];
  for (i = first; i <= last; i += 1) {
    array.push(i);
  }
  return array;
}

/**
 * Determines if a value is indivisible by the context `this`.
 * @param {number} value - The value to check.
 * @returns {boolean} True if `value` is indivisible by `this`, false otherwise.
 */
function indivisibleBy(value) {
  return value % this !== 0;
}

/**
 * Finds all prime numbers up to `n` using the Sieve of Eratosthenes algorithm.
 * @param {number} n - The upper limit of the range to find primes in.
 * @returns {number[]} An array of prime numbers up to `n`.
 */
function sieve(n) {
  if (n <= 1) {
    return [];
  }
  let prime;
  let possibilities;
  const primes = [];

  possibilities = newArrayWithRange(2, n);

  do {
    prime = possibilities.shift();
    primes.push(prime);
    possibilities = possibilities.filter(indivisibleBy, prime);
  } while (possibilities.length > 0);

  return primes;
}

/**
 * Exported function to find prime numbers up to `n`.
 * @param {number} n - The upper limit of the range to find primes in.
 * @returns {number[]} An array of prime numbers up to `n`.
 */
export const primes = (n) => sieve(n);

/**
 * Calculates the sum of all multiples of the given numbers below a specified limit.
 *
 * @param {number[]} multiples - An array of numbers to find multiples of.
 * @param {number} limit - The upper limit (exclusive) up to which to calculate the sum.
 * @returns {number} - The sum of all multiples of the given numbers below the limit.
 */
export function sum(multiples, limit) {
  let result = 0;
  for (let index = 1; index < limit; index++) {
    if (multiples.some((multiple) => index % multiple === 0)) {
      result += index;
    }
  }
  return result;
};

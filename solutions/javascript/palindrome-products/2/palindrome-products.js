/**
 * Class representing palindrome products.
 */
export class Palindromes {
  /**
   * Generate the largest and smallest palindromes within a range of factors.
   * @param {Object} params - The parameters for generating palindromes.
   * @param {number} params.minFactor - The minimum factor.
   * @param {number} params.maxFactor - The maximum factor.
   * @returns {Object} An object containing the largest and smallest palindromes with their factors.
   */
  static generate({ minFactor, maxFactor }) {
    if (minFactor > maxFactor) {
      throw new Error('min must be <= max');
    }

    /**
     * Check if a number is a palindrome.
     * @param {number} n - The number to check.
     * @returns {boolean} True if the number is a palindrome, false otherwise.
     */
    const isPalindrome = (n) => {
      const str = n.toString();
      return str === str.split('').reverse().join('');
    };

    /**
     * Search for palindromes in a specified range.
     * @param {number} n - The starting number.
     * @param {Function} pred - A predicate function to determine the search condition.
     * @param {Function} fn - A function to modify the number during the search.
     * @returns {Object} An object containing the palindrome value and its factors.
     */
    const search = (n, pred, fn) => {
      while (pred(n)) {
        if (!isPalindrome(n)) {
          n = fn(n);
          continue;
        }

        const factors = [];
        for (let p = minFactor; p <= Math.sqrt(n); p++) {
          if (n % p === 0 && n / p <= maxFactor) {
            factors.push([p, n / p]);
          }
        }

        if (factors.length > 0) {
          return { value: n, factors };
        }

        n = fn(n);
      }

      return { value: null, factors: [] };
    };

    const lower = minFactor * minFactor;
    const upper = maxFactor * maxFactor;

    return {
      largest: search(upper, (n) => n >= lower, (x) => x - 1),
      smallest: search(lower, (n) => n <= upper, (x) => x + 1),
    };
  }
}

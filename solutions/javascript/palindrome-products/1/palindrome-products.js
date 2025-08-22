/**
 * Reverse a given string.
 * @param {string} str - The string to reverse.
 * @returns {string} The reversed string.
 */
const reverseString = (str) => str.split('').reverse().join('');

/**
 * Class representing a Palindrome.
 */
class Palindrome {
  /**
   * Create a Palindrome.
   * @param {number} factor1 - The first factor.
   * @param {number} factor2 - The second factor.
   */
  constructor(factor1, factor2) {
    this.value = factor1 * factor2;
    this.factors = [[factor1, factor2]];
  }

  /**
   * Add more factors to the palindrome.
   * @param {number[]} factors - The factors to add.
   * @returns {Palindrome} The updated palindrome.
   */
  addFactors(factors) {
    this.factors.push(factors);
    return this;
  }

  /**
   * Check if the palindrome is valid.
   * @returns {boolean} True if the palindrome is valid, false otherwise.
   */
  isValid() {
    const strValue = `${this.value}`;
    return strValue === reverseString(strValue);
  }

  /**
   * Merge another palindrome's factors into this one.
   * @param {Palindrome} other - The other palindrome.
   * @returns {Palindrome} The merged palindrome.
   */
  merge(other) {
    other.factors.forEach((factorPair) => {
      this.factors.push(factorPair);
    });
    return this;
  }
}

/**
 * Class representing a collection of Palindromes.
 */
export class Palindromes {
  /**
   * Create a collection of palindromes.
   * @param {number} maxFactor - The maximum factor.
   * @param {number} [minFactor=1] - The minimum factor.
   */
  constructor(maxFactor, minFactor = 1) {
    this.maxFactor = maxFactor;
    this.minFactor = minFactor;
  }

  /**
   * Get the largest palindrome.
   * @returns {Palindrome|object} The largest palindrome or an object with null value and empty factors if not found.
   */
  get largest() {
    let left = this.maxFactor;
    let right = this.maxFactor;
    let largestPalindrome = new Palindrome(this.minFactor, this.minFactor);

    while (right >= this.minFactor) {
      let currentPalindrome = new Palindrome(left, right);

      if (largestPalindrome.value && currentPalindrome.value < largestPalindrome.value) {
        right--;
        left = right;
        continue;
      }

      if (currentPalindrome.isValid()) {
        if (largestPalindrome.value < currentPalindrome.value) {
          largestPalindrome = currentPalindrome;
        } else if (largestPalindrome.value === currentPalindrome.value) {
          largestPalindrome.merge(currentPalindrome);
        }
      }

      if (left <= this.minFactor) {
        right--;
        left = right;
      } else {
        left--;
      }
    }

    return largestPalindrome.isValid() ? largestPalindrome : { value: null, factors: [] };
  }

  /**
   * Get the smallest palindrome.
   * @returns {Palindrome|object} The smallest palindrome or an object with null value and empty factors if not found.
   */
  get smallest() {
    for (let factor1 = this.minFactor; factor1 <= this.maxFactor; factor1++) {
      for (let factor2 = factor1; factor2 <= this.maxFactor; factor2++) {
        const palindrome = new Palindrome(factor1, factor2);
        if (palindrome.isValid()) {
          return palindrome;
        }
      }
    }
    return { value: null, factors: [] };
  }

  /**
   * Generate a collection of palindromes based on the given parameters.
   * @param {object} params - The parameters for generating palindromes.
   * @param {number} params.maxFactor - The maximum factor.
   * @param {number} [params.minFactor=1] - The minimum factor.
   * @returns {Palindromes} The generated collection of palindromes.
   * @throws Will throw an error if the minimum factor is greater than the maximum factor.
   */
  static generate(params) {
    const minFactor = params.minFactor || 1;
    if (minFactor > params.maxFactor) {
      throw new Error('min must be <= max');
    }
    return new Palindromes(params.maxFactor, minFactor);
  }
}

/**
 * Class representing a candidate solution.
 */
class Candidate {
  constructor() {
    this.wasSearched = false;
    this.coins = [];
  }

  /**
   * Marks the candidate as searched.
   */
  searched() {
    this.wasSearched = true;
  }

  /**
   * Checks if the candidate has been searched.
   * @returns {boolean} True if the candidate has been searched, false otherwise.
   */
  isSearched() {
    return this.wasSearched;
  }

  /**
   * Gets the list of coins in the candidate.
   * @returns {number[]} The list of coins.
   */
  getCoins() {
    return this.coins;
  }

  /**
   * Adds a coin to the candidate and sorts the coins.
   * @param {number} coin - The coin to add.
   */
  addCoin(coin) {
    this.coins.push(coin);
    this.coins.sort((a, b) => a - b);
  }

  /**
   * Gets the count of coins in the candidate.
   * @returns {number} The count of coins.
   */
  getCoinCount() {
    return this.coins.length;
  }

  /**
   * Gets the sum of coins in the candidate.
   * @returns {number} The sum of coins.
   */
  getSum() {
    return this.coins.reduce((total, num) => total + num, 0);
  }
}

/**
 * Class representing a change calculator.
 */
export class Change {
  constructor() {
    this.candidates = [];
  }

  /**
   * Calculates the minimum coins needed to achieve the target amount.
   * @param {number[]} coinArray - Array of available coin denominations.
   * @param {number} target - The target amount to achieve.
   * @returns {number[]} The minimum coins needed to achieve the target amount.
   * @throws Will throw an error if the target is negative or cannot be represented with the given coins.
   */
  calculate(coinArray, target) {
    const { candidates } = this;
    candidates[target] = 0;
    candidates.fill(0);

    const isNumber = (element) => typeof element === 'number';

    /**
     * Saves a new candidate to the candidates array.
     * @param {Candidate} candidate - The candidate to save.
     */
    const saveCandidate = (candidate) => {
      const sum = candidate.getSum();

      if (sum <= target) {
        if (!isNumber(candidates[sum])) {
          if (candidates[sum].getCoinCount() > candidate.getCoinCount()) {
            candidates[sum] = candidate;
          }
        } else {
          candidates[sum] = candidate;
        }
      }
    };

    /**
     * Initializes the candidate array with the given coins only.
     */
    const initialize = () => {
      coinArray.forEach((coin) => {
        const candidate = new Candidate();
        candidate.addCoin(coin);
        saveCandidate(candidate);
      });
    };

    /**
     * Checks if all candidates have been searched.
     * @returns {boolean} True if all candidates have been searched, false otherwise.
     */
    const isDone = () =>
      candidates.every((candidate) => isNumber(candidate) || candidate.isSearched());

    /**
     * Gets the next unsearched member of the candidate array.
     * @returns {Candidate} The next unsearched candidate.
     */
    const getNext = () =>
      candidates.find((candidate) => !isNumber(candidate) && !candidate.isSearched());

    /**
     * Generates another candidate for each of the possible coins.
     * @param {Candidate} current - The current candidate.
     */
    const branch = (current) => {
      coinArray.forEach((coin) => {
        const candidate = new Candidate();
        current.getCoins().forEach((currentCoin) => {
          candidate.addCoin(currentCoin);
        });
        candidate.addCoin(coin);
        saveCandidate(candidate);
      });
    };

    if (target === 0) return [];

    if (target < 0) {
      throw new Error('Negative totals are not allowed.');
    }

    if (target < Math.min.apply(null, coinArray)) {
      throw new Error(`The total ${target} cannot be represented in the given currency.`);
    }

    initialize();

    while (!isDone()) {
      const candidate = getNext();
      branch(candidate);
      candidate.searched();
    }

    if (!isNumber(candidates[target])) return candidates[target].getCoins();
    throw new Error(`The total ${target} cannot be represented in the given currency.`);
  }
}

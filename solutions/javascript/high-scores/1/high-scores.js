/**
 * Class representing a collection of high scores.
 */
export class HighScores {
  /**
   * Create a new instance of HighScores.
   * @param {number[]} scores - The list of scores.
   */
  constructor(scores) {
    /**
     * @property {number[]} scores - The list of scores.
     */
    this.scores = scores;
  }

  /**
   * Get the latest score.
   * @return {number} The latest score.
   */
  get latest() {
    return this.scores[this.scores.length - 1];
  }

  /**
   * Get the personal best score.
   * @return {number} The highest score.
   */
  get personalBest() {
    return Math.max(...this.scores);
  }

  /**
   * Get the top three personal scores.
   * @return {number[]} An array of the top three scores, sorted in descending order.
   */
  get personalTopThree() {
    return [...this.scores]
      .sort((a, b) => b - a)
      .slice(0, 3);
  }
}
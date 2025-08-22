/**
 * Represents a series of numbers and provides methods to work with slices of the series.
 */
export class Series {
  /**
   * Creates an instance of Series.
   * @param {string} numberString - A string of digits representing the series.
   * @throws {Error} If the numberString is empty.
   */
  constructor(numberString) {
    if (!numberString) {
      throw new Error('series cannot be empty');
    }

    this.numberString = numberString;
    this.digits = this.getDigits();
  }

  /**
   * Converts the number string into an array of digits.
   * @returns {number[]} An array of digits.
   */
  getDigits() {
    return [...this.numberString].map((digit) => {
      const parsedDigit = parseInt(digit, 10);
      if (isNaN(parsedDigit)) {
        throw new Error('Invalid digit in series');
      }
      return parsedDigit;
    });
  }

  /**
   * Generates all possible slices of a given size from the series.
   * @param {number} sliceSize - The size of each slice.
   * @returns {number[][]} An array of slices, where each slice is an array of digits.
   * @throws {Error} If the sliceSize is negative, zero, or greater than the series length.
   */
  slices(sliceSize) {
    if (sliceSize < 0) {
      throw new Error('slice length cannot be negative');
    }
    
    if (!sliceSize) {
      throw new Error('slice length cannot be zero');
    }

    if (sliceSize > this.digits.length) {
      throw new Error('slice length cannot be greater than series length');
    }

    const result = [];
    for (let i = 0; i <= this.digits.length - sliceSize; i++) {
      const slice = this.digits.slice(i, i + sliceSize);
      result.push(slice);
    }

    return result;
  }
}
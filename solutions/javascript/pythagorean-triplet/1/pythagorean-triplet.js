/**
 * Class representing a Pythagorean triplet.
 */
class Triplet {
  /**
   * Create a triplet.
   * @param {number} a - The first number.
   * @param {number} b - The second number.
   * @param {number} c - The third number.
   */
  constructor(a, b, c) {
    this.a = a;
    this.b = b;
    this.c = c;
  }

  /**
   * Convert the triplet to an array.
   * @return {number[]} The array representation of the triplet.
   */
  toArray() {
    return [this.a, this.b, this.c];
  }

  /**
   * Check if the triplet is a Pythagorean triplet.
   * @return {boolean} True if it's a Pythagorean triplet, otherwise false.
   */
  get pythagorean() {
    return this.a * this.a + this.b * this.b === this.c * this.c;
  }

  /**
   * Get the sum of the triplet.
   * @return {number} The sum of the triplet.
   */
  get sum() {
    return this.a + this.b + this.c;
  }
}

/**
 * Generate Pythagorean triplets within a given range.
 * @param {Object} options - The options object.
 * @param {number} [options.minFactor=1] - The minimum factor to consider.
 * @param {number} [options.maxFactor] - The maximum factor to consider.
 * @param {number} [options.sum] - The desired sum of the triplet.
 * @return {Triplet[]} An array of Pythagorean triplets.
 */
export function triplets({ minFactor = 1, maxFactor, sum }) {
  const min = minFactor;
  const max = maxFactor || sum - 1;

  /**
   * Check if the triplet meets the desired conditions.
   * @param {Triplet} triplet - The triplet to check.
   * @return {boolean} True if the triplet meets the conditions, otherwise false.
   */
  const isDesired = (triplet) => {
    return triplet.pythagorean && (!sum || triplet.sum === sum);
  };

  const result = [];
  const squaredMap = {};

  // Precompute squares for efficiency
  for (let a = min; a < max; a += 1) {
    squaredMap[a * a] = a;
  }

  // Generate triplets
  for (let a = min; a < max - 1; a += 1) {
    for (let b = a + 1; b < max; b += 1) {
      const cSquared = a * a + b * b;
      if (squaredMap[cSquared]) {
        const triplet = new Triplet(a, b, squaredMap[cSquared]);
        if (isDesired(triplet)) {
          result.push(triplet);
        }
      }
    }
  }

  return result;
}

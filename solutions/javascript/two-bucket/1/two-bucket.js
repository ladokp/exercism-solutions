/**
 * Class representing a two bucket problem solver.
 */
export class TwoBucket {
  /**
   * Creates a two bucket problem solver.
   * @param {number} size1 - The size of the first bucket.
   * @param {number} size2 - The size of the second bucket.
   * @param {number} goal - The goal amount of water.
   * @param {string} start - The bucket to start with ('one' or 'two').
   */
  constructor(size1, size2, goal, start) {
    this.goal = goal;
    this.buckets = [new Bucket('one', size1), new Bucket('two', size2)];

    if (start === 'two') {
      this.buckets.reverse();
    }

    this.validate();
  }

  /**
   * Getter for the first bucket.
   * @returns {Bucket} The first bucket.
   */
  get first() {
    return this.buckets[0];
  }

  /**
   * Getter for the second bucket.
   * @returns {Bucket} The second bucket.
   */
  get second() {
    return this.buckets[1];
  }

  /**
   * Validates the goal.
   * @throws Will throw an error if the goal is not achievable.
   */
  validate() {
    if (this.goal > Math.max(this.first.size, this.second.size)) {
      throw new Error('Goal is bigger than the largest bucket.');
    }

    if (this.goal % gcd(this.first.size, this.second.size) !== 0) {
      throw new Error(
        'Goal must be a multiple of the GCD of the sizes of the two buckets.',
      );
    }
  }

  /**
   * Solves the two bucket problem.
   * @returns {Object} The solution containing moves, goal bucket, and other bucket amount.
   */
  solve() {
    this.first.empty();
    this.second.empty();
    let moves = 0;

    // fill the start bucket with the first move
    this.first.fill();
    moves += 1;

    // optimization: if the other bucket is the right size,
    // fill it immediately with the second move
    if (this.second.size === this.goal) {
      this.second.fill();
      moves += 1;
    }

    while (true) {
      if (this.first.amount === this.goal) {
        return {
          moves: moves,
          goalBucket: this.first.name,
          otherBucket: this.second.amount,
        };
      }

      if (this.second.amount === this.goal) {
        return {
          moves: moves,
          goalBucket: this.second.name,
          otherBucket: this.first.amount,
        };
      }

      if (this.first.isEmpty) {
        this.first.fill();
      } else if (this.second.isFull) {
        this.second.empty();
      } else {
        this.first.pourInto(this.second);
      }

      moves += 1;
    }
  }
}

/**
 * Class representing a bucket.
 */
class Bucket {
  /**
   * Creates a bucket.
   * @param {string} name - The name of the bucket.
   * @param {number} size - The size of the bucket.
   */
  constructor(name, size) {
    this.name = name;
    this.size = size;
    this.amount = 0;
  }

  /**
   * Getter for the available amount of space in the bucket.
   * @returns {number} The available amount.
   */
  get available() {
    return this.size - this.amount;
  }

  /**
   * Getter to check if the bucket is full.
   * @returns {boolean} True if the bucket is full, otherwise false.
   */
  get isFull() {
    return this.amount === this.size;
  }

  /**
   * Getter to check if the bucket is empty.
   * @returns {boolean} True if the bucket is empty, otherwise false.
   */
  get isEmpty() {
    return this.amount === 0;
  }

  /**
   * Fills the bucket to its capacity.
   */
  fill() {
    this.amount = this.size;
  }

  /**
   * Empties the bucket.
   */
  empty() {
    this.amount = 0;
  }

  /**
   * Pours water from this bucket into another bucket.
   * @param {Bucket} other - The other bucket to pour into.
   */
  pourInto(other) {
    const quantity = Math.min(this.amount, other.available);
    this.amount -= quantity;
    other.amount += quantity;
  }
}

/**
 * Calculates the greatest common divisor (GCD) of two numbers.
 * @param {number} a - The first number.
 * @param {number} b - The second number.
 * @returns {number} The GCD of the two numbers.
 */
const gcd = (a, b) => (b === 0 ? a : gcd(b, a % b));

/**
 * Class representing a rational number.
 */
class Rational {
  /**
   * Create a rational number.
   * @param {number} numerator - The numerator of the rational number.
   * @param {number} denominator - The denominator of the rational number.
   * @throws Will throw an error if the denominator is zero.
   */
  constructor(numerator, denominator) {
    if (denominator === 0) {
      throw new Error('Denominator must not be zero.');
    }

    this.numerator = numerator;
    this.denominator = denominator;

    this.reduce();
    this.ensureSignInNumerator();
  }

  /**
   * Add two rational numbers.
   * @param {Rational} that - The rational number to add.
   * @returns {Rational} The sum of the two rational numbers.
   */
  add(that) {
    const commonDenominator = this.denominator * that.denominator;
    return new Rational(
      this.numerator * that.denominator + that.numerator * this.denominator,
      commonDenominator,
    );
  }

  /**
   * Subtract one rational number from another.
   * @param {Rational} that - The rational number to subtract.
   * @returns {Rational} The difference of the two rational numbers.
   */
  sub(that) {
    const commonDenominator = this.denominator * that.denominator;
    return new Rational(
      this.numerator * that.denominator - that.numerator * this.denominator,
      commonDenominator,
    );
  }

  /**
   * Multiply two rational numbers.
   * @param {Rational} that - The rational number to multiply.
   * @returns {Rational} The product of the two rational numbers.
   */
  mul(that) {
    return new Rational(
      this.numerator * that.numerator,
      this.denominator * that.denominator,
    );
  }

  /**
   * Divide one rational number by another.
   * @param {Rational} that - The rational number to divide by.
   * @returns {Rational} The quotient of the two rational numbers.
   */
  div(that) {
    return new Rational(
      this.numerator * that.denominator,
      this.denominator * that.numerator,
    );
  }

  /**
   * Get the absolute value of the rational number.
   * @returns {Rational} The absolute value of the rational number.
   */
  abs() {
    return new Rational(Math.abs(this.numerator), Math.abs(this.denominator));
  }

  /**
   * Raise the rational number to an integer power.
   * @param {number} n - The exponent.
   * @returns {Rational} The rational number raised to the power n.
   */
  exprational(n) {
    return new Rational(
      Math.pow(this.numerator, n),
      Math.pow(this.denominator, n),
    );
  }

  /**
   * Raise a real number to the power of the rational number.
   * @param {number} base - The base real number.
   * @returns {number} The result of the base raised to the power of the rational number.
   */
  expreal(base) {
    return Math.pow(
      10.0,
      Math.log10(Math.pow(base, this.numerator)) / this.denominator,
    );
  }

  /**
   * Reduce the rational number to its simplest form.
   * @returns {Rational} The reduced rational number.
   */
  reduce() {
    const commonDivisor = this.gcd(this.numerator, this.denominator);

    this.numerator /= commonDivisor;
    this.denominator /= commonDivisor;
    this.ensureSignInNumerator();

    return this;
  }

  /**
   * Calculate the greatest common divisor of two numbers.
   * @param {number} a - The first number.
   * @param {number} b - The second number.
   * @returns {number} The greatest common divisor of a and b.
   */
  gcd(a, b) {
    let localA = a;
    let localB = b;
    while (localB !== 0) {
      const t = localB;
      localB = localA % localB;
      localA = t;
    }
    return localA;
  }

  /**
   * Ensure the sign is in the numerator.
   */
  ensureSignInNumerator() {
    if (this.denominator < 0) {
      this.denominator = -this.denominator;
      this.numerator = -this.numerator;
    }
  }
}

export { Rational };

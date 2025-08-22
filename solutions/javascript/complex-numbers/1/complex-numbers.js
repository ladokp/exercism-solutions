/**
 * Class representing a complex number.
 */
export class ComplexNumber {
  /**
   * Create a complex number.
   * @param {number} real - The real part of the complex number.
   * @param {number} imag - The imaginary part of the complex number.
   * @throws Will throw an error if real or imag is not a number.
   */
  constructor(real, imag) {
    if (typeof real !== 'number' || typeof imag !== 'number') {
      throw new TypeError('Real and imaginary parts must be numbers');
    }
    this.real = real;
    this.imag = imag;
  }

  /**
   * Add a complex number to this complex number.
   * @param {ComplexNumber} other - The complex number to add.
   * @returns {ComplexNumber} The result of the addition.
   */
  add(other) {
    return new ComplexNumber(this.real + other.real, this.imag + other.imag);
  }

  /**
   * Subtract a complex number from this complex number.
   * @param {ComplexNumber} other - The complex number to subtract.
   * @returns {ComplexNumber} The result of the subtraction.
   */
  sub(other) {
    return new ComplexNumber(this.real - other.real, this.imag - other.imag);
  }

  /**
   * Multiply this complex number by another complex number.
   * @param {ComplexNumber} other - The complex number to multiply by.
   * @returns {ComplexNumber} The result of the multiplication.
   */
  mul(other) {
    return new ComplexNumber(
      this.real * other.real - this.imag * other.imag,
      this.imag * other.real + this.real * other.imag
    );
  }

  /**
   * Divide this complex number by another complex number.
   * @param {ComplexNumber} other - The complex number to divide by.
   * @returns {ComplexNumber} The result of the division.
   * @throws Will throw an error if division by zero occurs.
   */
  div(other) {
    const denominator = other.real * other.real + other.imag * other.imag;
    if (denominator === 0) {
      throw new Error('Division by zero');
    }
    return new ComplexNumber(
      (this.real * other.real + this.imag * other.imag) / denominator,
      (this.imag * other.real - this.real * other.imag) / denominator
    );
  }

  /**
   * Get the magnitude (absolute value) of this complex number.
   * @returns {number} The magnitude of the complex number.
   */
  get abs() {
    return Math.sqrt(this.real * this.real + this.imag * this.imag);
  }

  /**
   * Get the conjugate of this complex number.
   * @returns {ComplexNumber} The conjugate of the complex number.
   */
  get conj() {
    return new ComplexNumber(this.real, this.imag !== 0 ? -this.imag : 0);
  }

  /**
   * Get the exponential of this complex number.
   * @returns {ComplexNumber} The exponential of the complex number.
   */
  get exp() {
    return new ComplexNumber(
      Math.exp(this.real) * Math.cos(this.imag),
      Math.exp(this.real) * Math.sin(this.imag)
    );
  }
}
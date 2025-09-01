/**
 * List of color codes for resistors.
 * @constant {string[]}
 */
const COLORS = [
  'black', 'brown', 'red', 'orange', 'yellow', 
  'green', 'blue', 'violet', 'grey', 'white'
];

/**
 * Represents one kilo-ohm in ohms.
 * @constant {number}
 */
const ONE_KILOOHM = 1000;

/**
 * Represents one mega-ohm in ohms.
 * @constant {number}
 */
const ONE_MEGAOHM = 1000000;

/**
 * Represents one giga-ohm in ohms.
 * @constant {number}
 */
const ONE_GIGAOHM = 1000000000;

/**
 * Custom error for invalid arguments.
 * @extends {Error}
 */
class ArgumentError extends Error {}

/**
 * Class representing a ResistorColorTrio.
 */
export class ResistorColorTrio {
  /**
   * Create a ResistorColorTrio.
   * @param {string[]} colors - The colors of the resistor bands.
   */
  constructor([tens, ones, zeros]) {
    /**
     * @property {string} tens - The tens color band.
     * @property {string} ones - The ones color band.
     * @property {string} zeros - The multiplier color band.
     */
    this.tens = tens;
    this.ones = ones;
    this.zeros = zeros;
  }

  /**
   * Get the resistor value.
   * @throws {ArgumentError} Throws an error if the colors are invalid.
   * @return {number} The value of the resistor.
   */
  get value() {
    if (!this.isValid) {
      throw new ArgumentError('invalid color');
    }
    return this.significants() * this.multiplier();
  }

  /**
   * Get the resistor label.
   * @return {string} The label of the resistor.
   */
  get label() {
    return `Resistor value: ${this}`;
  }

  /**
   * Check if the color bands are valid.
   * @return {boolean} True if the color bands are valid, otherwise false.
   */
  get isValid() {
    return (
      COLORS.indexOf(this.tens) > -1 &&
      COLORS.indexOf(this.ones) > -1 &&
      COLORS.indexOf(this.zeros) > -1
    );
  }

  /**
   * Convert the resistor value to a string with appropriate units.
   * @return {string} The resistor value as a string.
   */
  toString() {
    const units = [
      { limit: 1e9, divisor: 1e9, suffix: "gigaohms" },
      { limit: 1e6, divisor: 1e6, suffix: "megaohms" },
      { limit: 1e3, divisor: 1e3, suffix: "kiloohms" },
      { limit: 0, divisor: 1, suffix: "ohms" },
    ];
  
    const { divisor, suffix } = units.find(u => this.value >= u.limit);
  
    // Use up to 2 decimals for better readability
    const formatted = (this.value / divisor).toFixed(2).replace(/\.00$/, "");
  
    return `${formatted} ${suffix}`;
  }

  /**
   * Calculate the significant digits of the resistor value.
   * @private
   * @return {number} The significant digits.
   */
  significants() {
    return this.colorCode(this.tens) * 10 + this.colorCode(this.ones);
  }

  /**
   * Calculate the multiplier for the resistor value.
   * @private
   * @return {number} The multiplier.
   */
  multiplier() {
    return Math.pow(10, this.colorCode(this.zeros));
  }

  /**
   * Get the numeric value of a color code.
   * @private
   * @param {string} color - The color code.
   * @return {number} The numeric value of the color code.
   */
  colorCode(color) {
    return COLORS.indexOf(color);
  }
}

/**
 * A class that represents a 24-hour clock.
 */
export class Clock {
  /**
   * Creates a new Clock instance.
   * @param {number} hour - The initial hour (0-23).
   * @param {number} minute - The initial minute (0-59).
   */
  constructor(hour = 0, minute = 0) {
    this.hour = this.normalizeHour(hour + Math.floor(minute / 60));
    this.minute = ((minute % 60) + 60) % 60;
  }

  /**
   * Normalizes the hour to a 24-hour format.
   * @param {number} hour - The hour to normalize.
   * @returns {number} - The normalized hour (0-23).
   */
  normalizeHour(hour) {
    return ((hour % 24) + 24) % 24;
  }

  /**
   * Returns the time as a string in HH:MM format.
   * @returns {string} - The time as a string.
   */
  toString() {
    const hourStr = this.hour.toString().padStart(2, '0');
    const minuteStr = this.minute.toString().padStart(2, '0');
    return `${hourStr}:${minuteStr}`;
  }

  /**
   * Adds minutes to the current time.
   * @param {number} mins - The number of minutes to add.
   * @returns {Clock} - The updated Clock instance.
   */
  plus(mins) {
    const totalMinutes = this.minute + mins;
    this.hour = this.normalizeHour(this.hour + Math.floor(totalMinutes / 60));
    this.minute = totalMinutes % 60;
    return this;
  }

  /**
   * Subtracts minutes from the current time.
   * @param {number} mins - The number of minutes to subtract.
   * @returns {Clock} - The updated Clock instance.
   */
  minus(mins) {
    const totalMinutes = this.minute - mins;
    this.hour = this.normalizeHour(this.hour + Math.floor(totalMinutes / 60));
    this.minute = ((totalMinutes % 60) + 60) % 60;
    return this;
  }

  /**
   * Compares this clock to another clock for equality.
   * @param {Clock} other - The other Clock instance to compare.
   * @returns {boolean} - True if the clocks are equal, otherwise false.
   */
  equals(other) {
    return this.hour === other.hour && this.minute === other.minute;
  }
}
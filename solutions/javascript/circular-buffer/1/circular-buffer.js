/**
 * Error thrown when attempting to read from an empty buffer.
 */
export class BufferEmptyError extends Error {
  /**
   * Create a BufferEmptyError.
   * @param {string} [message='Buffer is empty.'] - Error message.
   */
  constructor(message = 'Buffer is empty.') {
    super(message);
    this.name = 'BufferEmptyError';
  }
}

/**
 * Error thrown when attempting to write to a full buffer.
 */
export class BufferFullError extends Error {
  /**
   * Create a BufferFullError.
   * @param {string} [message='Buffer is full.'] - Error message.
   */
  constructor(message = 'Buffer is full.') {
    super(message);
    this.name = 'BufferFullError';
  }
}

/**
 * Circular buffer data structure.
 */
export default class CircularBuffer {
  /**
   * Create a CircularBuffer.
   * @param {number} capacity - Maximum capacity of the buffer.
   */
  constructor(capacity) {
    /** @type {Array} */
    this.buffer = [];
    /** @type {number} */
    this.bufferMax = capacity;
  }

  /**
   * Read the oldest value from the buffer.
   * @returns {*} The oldest value in the buffer.
   * @throws {BufferEmptyError} If the buffer is empty.
   */
  read() {
    if (this.buffer.length === 0) {
      throw new BufferEmptyError();
    }
    return this.buffer.splice(0, 1)[0];
  }

  /**
   * Write a value to the buffer.
   * @param {*} value - The value to write.
   * @returns {number|null} The new length of the buffer, or null if no value was written.
   * @throws {BufferFullError} If the buffer is full.
   */
  write(value) {
    if (this.buffer.length === this.bufferMax) {
      throw new BufferFullError();
    }
    return value !== undefined && value !== null ? this.buffer.push(value) : null;
  }

  /**
   * Forcefully write a value to the buffer, overwriting the oldest value if the buffer is full.
   * @param {*} value - The value to write.
   */
  forceWrite(value) {
    if (this.buffer.length === this.bufferMax) {
      this.read();
    }
    this.write(value);
  }

  /**
   * Clear the buffer.
   * @returns {Array} The cleared buffer.
   */
  clear() {
    this.buffer = [];
    return this.buffer;
  }
}

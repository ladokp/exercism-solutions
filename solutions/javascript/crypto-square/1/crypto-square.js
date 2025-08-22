/**
 * Class representing a Crypto.
 */
export class Crypto {
  /**
   * Create a Crypto instance.
   * @param {string} input - The input string to be encrypted.
   */
  constructor(input) {
    this.input = input;
  }

  /**
   * Get the plaintext by removing non-alphanumeric characters and converting to lowercase.
   * @return {string} The sanitized plaintext.
   */
  get plaintext() {
    return this.input.toLowerCase().replace(/[^a-z0-9]/g, '');
  }

  /**
   * Get the ciphertext by creating segments and joining them with spaces.
   * @return {string} The ciphertext.
   */
  get ciphertext() {
    const chunkSize = this.size;
    if (chunkSize === 0) {
      return '';
    }

    const splitRegex = new RegExp(`.{1,${chunkSize}}`, 'g');
    return this.ciphertextSegments()
      .join('')
      .match(splitRegex)
      .map((item) => item.padEnd(chunkSize, ' '))
      .join(' ');
  }

  /**
   * Get the size of the square by calculating the ceiling of the square root of plaintext length.
   * @return {number} The size of the square.
   */
  get size() {
    const realLength = Math.sqrt(this.plaintext.length);
    return Math.ceil(realLength);
  }

  /**
   * Get the segments of the ciphertext by transposing the plaintext segments.
   * @return {string[]} The segments of the ciphertext.
   */
  ciphertextSegments() {
    const textSegments = this.plaintextSegments();
    const columns = Array.from({ length: this.size }, () => []);

    textSegments.forEach(segment => {
      segment.split('').forEach((letter, index) => {
        columns[index].push(letter);
      });
    });

    return columns.map(column => column.join(''));
  }

  /**
   * Get the segments of the plaintext by splitting it into chunks of size.
   * @return {string[]} The segments of the plaintext.
   */
  plaintextSegments() {
    const plainText = this.plaintext;
    const chunkSize = this.size;
    const splitRegex = new RegExp(`.{1,${chunkSize}}`, 'g');
    return plainText.match(splitRegex);
  }
}

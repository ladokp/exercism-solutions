// Constants
const ALPHA = 'abcdefghijklmnopqrstuvwxyz'; // Alphabet used for encoding and decoding

/**
 * Generates a random key consisting of 100 lowercase alphabetic characters.
 * @returns {string} - A random key string of length 100.
 */
function generateKey() {
  return Array.from({ length: 100 }, () => ALPHA[Math.floor(Math.random() * ALPHA.length)]).join('');
}

/**
 * Computes the mathematical modulo operation ensuring a positive result.
 * @param {number} n - The number to be reduced.
 * @param {number} m - The modulus.
 * @returns {number} - The positive modulo result.
 */
const mod = (n, m) => ((n % m) + m) % m;

/**
 * Encodes or decodes a given text using a key and a sign multiplier.
 * @param {string} key - The key to use for encoding or decoding.
 * @param {string} inText - The input text to be processed.
 * @param {number} sign - 1 for encoding, -1 for decoding.
 * @returns {string} - The encoded or decoded text.
 */
function xCode(key, inText, sign) {
  return [...inText].reduce((outText, letter, index) => {
    // Get the current key character and its position in the alphabet
    const keyCharIndex = ALPHA.indexOf(key[mod(index, key.length)]);
    if (keyCharIndex === -1) {
      throw new Error(`Invalid character in key: ${key[index]}`);
    }

    // Get the current input letter's position in the alphabet
    const letterIndex = ALPHA.indexOf(letter);
    if (letterIndex === -1) {
      throw new Error(`Invalid character in input text: ${letter}`);
    }

    // Compute the new letter's position based on the sign and key character offset
    const newIndex = mod(letterIndex + sign * keyCharIndex, ALPHA.length);

    // Append the corresponding letter to the output
    return outText + ALPHA[newIndex];
  }, '');
}

/**
 * Cipher class for encoding and decoding messages with a given key.
 */
export class Cipher {
  /**
   * Initializes a Cipher instance with a key. If no key is provided, a random key is generated.
   * @param {string} [key] - The key to use for encoding and decoding (optional).
   * @throws {Error} - Throws an error if the key contains non-lowercase letters or is empty.
   */
  constructor(key) {
    if (key === undefined) {
      this.key = generateKey();
    } else if (!/^[a-z]+$/.test(key)) {
      throw new Error('Bad key: Key must be a non-empty string containing only lowercase letters.');
    } else {
      this.key = key;
    }
  }

  /**
   * Encodes the given plain text using the instance's key.
   * @param {string} plainText - The text to encode.
   * @returns {string} - The encoded text.
   * @throws {Error} - Throws an error if the plain text contains invalid characters.
   */
  encode(plainText) {
    return xCode(this.key, plainText, 1);
  }

  /**
   * Decodes the given encoded text using the instance's key.
   * @param {string} encodedText - The text to decode.
   * @returns {string} - The decoded text.
   * @throws {Error} - Throws an error if the encoded text contains invalid characters.
   */
  decode(encodedText) {
    return xCode(this.key, encodedText, -1);
  }
}

const ALPHABET = 'abcdefghijklmnopqrstuvwxyz';
const ALPHABET_LENGTH = ALPHABET.length;

/**
 * Checks if two numbers are coprimes.
 * @param {number} a - First number.
 * @param {number} b - Second number.
 * @returns {boolean} True if a and b are coprimes, otherwise false.
 */
const areCoprimes = (a, b) => {
  for (let i = Math.min(a, b); i > 1; i--) {
    if (a % i === 0 && b % i === 0) {
      return false;
    }
  }
  return true;
};

/**
 * Ensures that two numbers are coprimes.
 * @param {number} a - First number.
 * @param {number} b - Second number.
 * @throws {Error} If a and b are not coprimes.
 */
const checkCoprime = (a, b) => {
  if (!areCoprimes(a, b)) {
    throw new Error('a and m must be coprime.');
  }
};

/**
 * Checks if a value is a number.
 * @param {any} candidate - The value to check.
 * @returns {boolean} True if the candidate is a number, otherwise false.
 */
const isNumber = (candidate) => !isNaN(Number(candidate));

/**
 * Finds the Modular Multiplicative Inverse (MMI) of a number.
 * @param {number} a - The number to find the MMI for.
 * @returns {number} The MMI of a.
 */
const findMMI = (a) => {
  let i = 1;
  while (true) {
    i++;
    if ((a * i - 1) % ALPHABET_LENGTH === 0) {
      return i;
    }
  }
};

/**
 * Computes the positive modulo of two numbers.
 * @param {number} a - The dividend.
 * @param {number} b - The divisor.
 * @returns {number} The positive modulo of a and b.
 */
const positiveModulo = (a, b) => ((a % b) + b) % b;

/**
 * Groups elements into sub-arrays of specified length.
 * @param {Array} elements - The elements to group.
 * @param {number} groupLength - The length of each group.
 * @returns {Array} The grouped elements.
 */
const groupBy = (elements, groupLength) => {
  const result = [[]];
  let i = 0;
  elements.forEach((el) => {
    if (result[i] && result[i].length < groupLength) {
      result[i].push(el);
    } else {
      i++;
      result[i] = [el];
    }
  });
  return result;
};

/**
 * Encodes a phrase using the affine cipher.
 * @param {string} phrase - The phrase to encode.
 * @param {Object} keys - The keys for the cipher.
 * @param {number} keys.a - The 'a' key.
 * @param {number} keys.b - The 'b' key.
 * @returns {string} The encoded phrase.
 */
export const encode = (phrase, { a, b }) => {
  checkCoprime(a, ALPHABET_LENGTH);
  let encodedText = '';

  phrase
    .toLowerCase()
    .split('')
    .filter((char) => char !== ' ')
    .forEach((char) => {
      if (ALPHABET.includes(char)) {
        const x = ALPHABET.indexOf(char);
        const encodedIndex = (a * x + b) % ALPHABET_LENGTH;
        encodedText += ALPHABET[encodedIndex];
      } else if (isNumber(char)) {
        encodedText += char;
      }
    });

  return groupBy(encodedText.split(''), 5)
    .map((group) => group.join(''))
    .join(' ');
};

/**
 * Decodes a phrase using the affine cipher.
 * @param {string} phrase - The phrase to decode.
 * @param {Object} keys - The keys for the cipher.
 * @param {number} keys.a - The 'a' key.
 * @param {number} keys.b - The 'b' key.
 * @returns {string} The decoded phrase.
 */
export const decode = (phrase, { a, b }) => {
  checkCoprime(a, ALPHABET_LENGTH);
  const mmi = findMMI(a);

  return phrase
    .split('')
    .filter((char) => char !== ' ')
    .map((char) => {
      if (isNumber(char)) {
        return char;
      }
      const y = ALPHABET.indexOf(char);
      const decodedIndex = positiveModulo(mmi * (y - b), ALPHABET_LENGTH);
      return ALPHABET[decodedIndex];
    })
    .join('');
};

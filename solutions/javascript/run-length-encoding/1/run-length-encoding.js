/**
 * Encodes a string using run-length encoding.
 *
 * @param {string} plainText - The string to encode.
 * @returns {string} The encoded string.
 */
export const encode = (plainText) => {
  const consecutiveChars = /([\w\s])\1*/g;
  return plainText.replace(consecutiveChars, (match) =>
    match.length > 1 ? match.length + match[0] : match[0]
  );
};

/**
 * Decodes a run-length encoded string.
 *
 * @param {string} encodedText - The encoded string to decode.
 * @returns {string} The decoded string.
 */
export const decode = (encodedText) => {
  const countAndChar = /(\d+)(\w|\s)/g;
  return encodedText.replace(countAndChar, (match, repeats, char) =>
    char.repeat(Number(repeats))
  );
};

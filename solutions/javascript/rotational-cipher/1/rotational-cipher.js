/**
 * Rotates the characters in the provided text by the given shift value.
 *
 * @param {string} text - The input text to be rotated.
 * @param {number} shift - The number of positions to shift each character.
 * @returns {string} - The rotated text.
 */
export function rotate(text, shift) {
  return [...text]
    .map((char) => {
      const isUpperCase = char.match(/[A-Z]/);
      const isAlphabetic = char.match(/[a-z]/i);
      const baseCharCode = (isUpperCase ? 'A' : 'a').charCodeAt(0);

      return isAlphabetic
        ? String.fromCharCode(
            ((char.charCodeAt(0) - baseCharCode + shift) % 26) + baseCharCode
          )
        : char;
    })
    .join('');
}

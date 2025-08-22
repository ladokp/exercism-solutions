/**
 * Parses a phrase to generate its acronym.
 * 
 * @param {string} phrase - The phrase to parse.
 * @returns {string} The generated acronym.
 */
function parse(phrase) {
  return phrase
    .replace("'", '')
    .match(/^[A-Z]|(?<=[^A-Z])[A-Z]|\b[a-z]/g)
    .join('')
    .toUpperCase();
}

export { parse };

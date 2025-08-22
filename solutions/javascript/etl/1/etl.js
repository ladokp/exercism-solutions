/**
 * Transforms the input object by mapping each item to its corresponding key as a number.
 *
 * @param {Object} input - An object where the keys are strings representing numbers 
 *                         and the values are arrays of strings.
 * @returns {Object} output - An object where the keys are the lowercase items from the 
 *                            input arrays and the values are the corresponding number keys.
 *
 * @example
 * const input = {
 *   "1": ["A", "E", "I", "O", "U"],
 *   "2": ["D", "G"],
 *   "3": ["B", "C", "M", "P"]
 * };
 * const output = transform(input);
 * // output will be:
 * // {
 * //   "a": 1,
 * //   "e": 1,
 * //   "i": 1,
 * //   "o": 1,
 * //   "u": 1,
 * //   "d": 2,
 * //   "g": 2,
 * //   "b": 3,
 * //   "c": 3,
 * //   "m": 3,
 * //   "p": 3
 * // }
 */
export const transform = (input) => {
  return Object.entries(input).reduce((output, [key, items]) => {
    items.forEach((item) => {
      output[item.toLowerCase()] = Number(key);
    });
    return output;
  }, {});
};
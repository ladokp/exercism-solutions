/**
 * Flattens a nested array.
 *
 * @param {Array} arr - The array to flatten.
 * @returns {Array} The flattened array.
 */
export const flatten = (arr) => {
  return arr
    .reduce(
      (acc, el) =>
        Array.isArray(el) ? acc.concat(flatten(el)) : acc.concat(el),
      [],
    )
    .filter((el) => el !== null && el !== undefined);
};

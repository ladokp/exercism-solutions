/**
 * Filters an array based on a provided filter function and a boolean flag.
 *
 * @param {Array} array - The array to filter.
 * @param {Function} filter - The filter function to apply to each element.
 * @param {boolean} keepMatches - If true, keeps elements that match the filter; otherwise, discards them.
 * @returns {Array} The filtered array.
 */
const strain = (array, filter, keepMatches) => {
  const results = [];
  array.forEach(item => {
    if (filter(item) === keepMatches) {
      results.push(item);
    }
  });
  return results;
};

/**
 * Keeps elements in an array that match the provided filter function.
 *
 * @param {Array} array - The array to filter.
 * @param {Function} filter - The filter function to apply to each element.
 * @returns {Array} The array with matching elements kept.
 */
export const keep = (array, filter) => strain(array, filter, true);

/**
 * Discards elements in an array that match the provided filter function.
 *
 * @param {Array} array - The array to filter.
 * @param {Function} filter - The filter function to apply to each element.
 * @returns {Array} The array with matching elements discarded.
 */
export const discard = (array, filter) => strain(array, filter, false);

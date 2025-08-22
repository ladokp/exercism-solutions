/**
 * Performs a binary search on a sorted array to find the index of a target value.
 * @param {number[]} sortedArray - A sorted array of numbers in ascending order.
 * @param {number} targetValue - The number to search for in the array.
 * @returns {number} The index of the targetValue in the array.
 * @throws {Error} If the targetValue is not found in the array.
 */
export const find = (sortedArray, targetValue) => {
  let left = 0;
  let right = sortedArray.length - 1;
  let mid;

  while (left <= right) {
    mid = Math.floor((left + right) / 2);
    if (sortedArray[mid] === targetValue) return mid;
    else if (sortedArray[mid] < targetValue) left = mid + 1;
    else right = mid - 1;
  }
  throw new Error('Value not in array');
};

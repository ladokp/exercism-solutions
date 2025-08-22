/**
 * @fileoverview Finding Eulerian Path in a set of dominoes.
 */

/**
 * Finds an Eulerian path in a set of dominoes.
 * @param {Array<Array<number>>} dominoes - The set of dominoes represented as pairs of numbers.
 * @returns {Array<Array<number>>|null} - The Eulerian path as an array of domino pairs or null if no path exists.
 */
export const chain = (dominoes) => {
  if (dominoes.length === 0) return [];

  let remainings = dominoes;
  let solution = [dominoes[0][0]];

  /**
   * Finds the first loop in the domino set.
   * @returns {boolean} - Whether a loop was found.
   */
  const findFirstLoop = () => {
    const maybeNextDomino = remainings.find(matchOneEnd(last(solution)));
    if (!maybeNextDomino) return false;
    remainings = removeOneDomino(maybeNextDomino, remainings);
    solution.push(otherEnd(last(solution), maybeNextDomino));
    if (last(solution) === solution[0]) return true;
    if (remainings.length === 0) return false;
    return findFirstLoop();
  };

  if (!findFirstLoop()) return null;

  let target = solution.length;

  /**
   * Backtracks the solution and joins new loops.
   * @returns {boolean} - Whether the remaining dominoes can form a complete path.
   */
  const backtrackAndJoinLoops = () => {
    if (target < 0) return remainings.length === 0;

    while (remainings.find(matchOneEnd(solution[target]))) {
      const firstHalf = solution.slice(0, target);
      const secondHalf = solution.slice(target + 1);

      solution = [solution[target]];
      if (!findFirstLoop()) return false;
      solution = [...firstHalf, ...solution, ...secondHalf];
    }

    target -= 1;
    return backtrackAndJoinLoops();
  };

  if (!backtrackAndJoinLoops()) return null;

  return solution
    .slice(0, solution.length - 1)
    .map((v, i) => [solution[i], solution[i + 1]]);
};

/**
 * Removes a specific domino from the set.
 * @param {Array<number>} domino - The domino to remove.
 * @param {Array<Array<number>>} dominoes - The set of dominoes.
 * @returns {Array<Array<number>>} - The new set of dominoes without the removed domino.
 */
const removeOneDomino = (domino, dominoes) => {
  const index = dominoes.findIndex(matchDomino(domino));
  return [...dominoes.slice(0, index), ...dominoes.slice(index + 1)];
};

/**
 * Gets the other end of a domino given one end.
 * @param {number} x - One end of the domino.
 * @param {Array<number>} domino - The domino represented as a pair of numbers.
 * @returns {number} - The other end of the domino.
 */
const otherEnd = (x, [a, b]) => (a === x ? b : a);

/**
 * Matches a domino exactly.
 * @param {Array<number>} domino - The domino to match.
 * @returns {Function} - A function that takes another domino and returns whether it matches the first.
 */
const matchDomino = ([a, b]) => ([c, d]) => (a === c && b === d) || (a === d && b === c);

/**
 * Matches a domino with one end.
 * @param {number} x - One end of the domino.
 * @returns {Function} - A function that takes a domino and returns whether one end matches x.
 */
const matchOneEnd = (x) => (domino) => domino.includes(x);

/**
 * Gets the last element of an array.
 * @param {Array<any>} array - The array to get the last element from.
 * @returns {any} - The last element of the array.
 */
const last = (array) => array[array.length - 1];

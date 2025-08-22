/**
 * Checks if the last argument is an options object.
 * @param {Array} args - The arguments passed to the function.
 * @returns {boolean} True if the last argument is an object, otherwise false.
 */
const lastArgIsOptions = (args) => {
  const last = args[args.length - 1];
  return typeof last === 'object' && !Array.isArray(last);
};

/**
 * Constructs the concluding line of the proverb.
 * @param {string} firstArg - The first argument passed to the proverb function.
 * @param {string} [qualifier=''] - An optional qualifier for the first argument.
 * @returns {string} The concluding line of the proverb.
 */
const conclusion = (firstArg, qualifier = '') =>
  `And all for the want of a ${qualifier}${firstArg}.`;

/**
 * Generates a proverb based on the given arguments.
 * @param {...string|Object} args - The items in the chain of events, followed by an optional options object.
 * @param {string} [args[].qualifier] - An optional qualifier for the first argument.
 * @returns {string} The generated proverb.
 */
export const proverb = (...args) => {
  let options = {};
  if (lastArgIsOptions(args)) {
    options = args.pop();
  }

  if (args.length === 0) {
    return '';
  }

  const chainOfEvents = args.slice(0, -1).map(
    (arg, index) => `For want of a ${arg} the ${args[index + 1]} was lost.`,
  );

  const qualifier = options.qualifier ? `${options.qualifier} ` : '';
  chainOfEvents.push(conclusion(args[0], qualifier));

  return chainOfEvents.join('\n');
};

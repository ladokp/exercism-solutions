// @ts-check

/**
 * Creates a sign with a congratulatory message.
 * @param {string} occasion - The occasion for the sign.
 * @param {string} name - The name of the person.
 * @returns {string} - The created sign.
 */
export function buildSign(occasion, name) {
  return `Happy ${occasion} ${name}!`;
}

/**
 * Creates a birthday sign with an age-dependent message.
 * @param {number} age - The age of the person.
 * @returns {string} - The created birthday sign.
 */
export function buildBirthdaySign(age) {
  const ageDescriptor = age >= 50 ? 'mature' : 'young';
  return `Happy Birthday! What a ${ageDescriptor} fellow you are.`;
}

/**
 * Creates a graduation sign.
 * @param {string} name - The name of the person.
 * @param {number} year - The graduation year.
 * @returns {string} - The created graduation sign.
 */
export function graduationFor(name, year) {
  return `Congratulations ${name}!\nClass of ${year}`;
}

/**
 * Calculates the cost of a sign based on its length.
 * @param {string} sign - The text of the sign.
 * @param {string} [currency='dollars'] - The currency.
 * @returns {string} - The calculated cost of the sign.
 */
export function costOf(sign, currency = 'dollars') {
  const cost = (sign.length * 2 + 20).toFixed(2);
  return `Your sign costs ${cost} ${currency}.`;
}

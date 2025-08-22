/**
 * Promisify a callback-based function.
 * @param {Function} fn - The function to promisify.
 * @returns {Function} A function that returns a promise.
 */
export const promisify = (fn) => (...args) => 
  new Promise((resolve, reject) => {
    fn(...args, (err, result) => (err ? reject(err) : resolve(result)));
  });

/**
 * Resolve all promises.
 * @param {Promise[]} promises - An array of promises.
 * @returns {Promise<[]>} A promise that resolves when all promises have resolved.
 */
export const all = (promises) => {
  if (!promises) return Promise.resolve();
  if (promises.length === 0) return Promise.resolve([]);
  return promises.reduce(
    async (acc, promise) => (await acc).concat(await promise),
    Promise.resolve([]),
  );
};

/**
 * Settle all promises.
 * @param {Promise[]} promises - An array of promises.
 * @returns {Promise<[]>} A promise that resolves when all promises have settled.
 */
export const allSettled = (promises) => {
  if (!promises) return Promise.resolve();
  if (promises.length === 0) return Promise.resolve([]);
  return promises.reduce(
    async (acc, promise) =>
      (await acc).concat(await promise.catch((err) => err)),
    Promise.resolve([]),
  );
};

/**
 * Race among promises.
 * @param {Promise[]} promises - An array of promises.
 * @returns {Promise<*>} A promise that resolves or rejects as soon as one of the promises resolves or rejects.
 */
export const race = (promises) => {
  if (!promises) return Promise.resolve();
  if (promises.length === 0) return Promise.resolve([]);
  return new Promise((resolve, reject) => {
    promises.forEach((promise) => {
      promise.then(resolve, reject);
    });
  });
};

/**
 * Resolve any promise.
 * @param {Promise[]} promises - An array of promises.
 * @returns {Promise<*>} A promise that resolves as soon as one of the promises resolves.
 */
export const any = (promises) => {
  if (!promises) return Promise.resolve();
  if (promises.length === 0) return Promise.resolve([]);
  return new Promise((resolve, reject) => {
    promises.forEach((promise) => {
      promise.then(resolve).catch(() => null);
    });
    allSettled(promises).then(reject);
  });
};

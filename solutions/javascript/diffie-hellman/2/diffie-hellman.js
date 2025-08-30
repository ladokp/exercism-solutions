/**
 * Check if a number is a prime number.
 *
 * A prime number is a number greater than 1 that is only divisible by 1 and itself.
 *
 * @param {number} number - The number to test for primality.
 * @returns {boolean} Returns `true` if the number is prime, otherwise `false`.
 */
const isPrime = (number) => {
  if (number <= 1) {
    return false;
  }
  if (number === 2 || number === 3) {
    return true;
  }
  if (number % 2 === 0) {
    return false;
  }
  for (let i = 3; i <= Math.sqrt(number); i += 2) {
    if (number % i === 0) {
      return false;
    }
  }
  return true;
};

/**
 * Class representing the Diffie-Hellman key exchange protocol.
 *
 * This class provides methods to generate public keys and shared secrets
 * using the Diffie-Hellman algorithm. The class assumes integer arithmetic.
 */
export class DiffieHellman {
  /**
   * Create a Diffie-Hellman instance.
   *
   * @param {number} p - A large prime number (modulus).
   * @param {number} g - A prime number (base/generator).
   * @throws {Error} Throws an error if either `p` or `g` is not a prime number
   *                 or falls outside the valid range for Diffie-Hellman.
   */
  constructor(p, g) {
    if (!DiffieHellman.validateInitialArguments(p, g)) {
      throw Error('Constructor arguments are out of range or non-prime!');
    }

    this.p = p;
    this.g = g;
  }

  /**
   * Generate the public key using a private key.
   *
   * @param {number} privateKey - A private key (must be > 1 and < p).
   * @returns {number} The corresponding public key.
   * @throws {Error} Throws an error if the private key is ≤ 1 or ≥ p.
   */
  getPublicKey(privateKey) {
    if (privateKey <= 1 || privateKey >= this.p) {
      throw Error(
        'Private key must be greater than 1 and less than the modulus parameter p!',
      );
    }

    return this.g ** privateKey % this.p;
  }

  /**
   * Generate a random private key for a given prime modulus.
   *
   * @param {number} p - The prime modulus.
   * @returns {number} A random private key in the range (1, p).
   */
  static getPrivateKey(p) {
    return Math.floor(Math.random() * (p - 2) + 2);
  }

  /**
   * Compute the shared secret using another party's public key and our private key.
   *
   * @param {number} theirPublicKey - The other party's public key.
   * @param {number} ourPrivateKey - Our own private key.
   * @returns {number} The shared secret value.
   */
  getSecret(theirPublicKey, ourPrivateKey) {
    return theirPublicKey ** ourPrivateKey % this.p;
  }

  /**
   * Validate the initial prime parameters `p` and `g`.
   *
   * @param {number} p - A prime number (modulus).
   * @param {number} g - A prime number (generator).
   * @returns {boolean} Returns `true` if both values are prime, otherwise `false`.
   */
  static validateInitialArguments(p, g) {
    return isPrime(p) && isPrime(g);
  }
}

/**
 * Check if a number is a prime number.
 * 
 * @param {number} number - The number to check for primality.
 * @returns {boolean} - Returns true if the number is prime, otherwise false.
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
 * Class representing the Diffie-Hellman key exchange.
 */
export class DiffieHellman {
  /**
   * Create a Diffie-Hellman key exchange instance.
   * @param {number} p - Prime number.
   * @param {number} g - Prime number.
   * @throws Will throw an error if p or g is out of range or non-prime.
   */
  constructor(p, g) {
    if (!DiffieHellman.validateInitialArguments(p, g)) {
      throw Error('Constructor arguments are out of range or non-prime!');
    }

    this.p = p;
    this.g = g;
  }

  /**
   * Get the public key based on the private key.
   * @param {number} privateKey - The private key.
   * @returns {number} - The public key.
   * @throws Will throw an error if the private key is out of range.
   */
  getPublicKey(privateKey) {
    if (privateKey <= 1 || privateKey > this.p - 1) {
      throw Error(
        'Private key a must be greater than one but less than modulus parameter p!',
      );
    }
    return this.g ** privateKey % this.p;
  }

  /**
   * Get the shared secret based on their public key and our private key.
   * @param {number} theirPublicKey - Their public key.
   * @param {number} ourPrivateKey - Our private key.
   * @returns {number} - The shared secret.
   */
  getSecret(theirPublicKey, ourPrivateKey) {
    return theirPublicKey ** ourPrivateKey % this.p;
  }

  /**
   * Validate the initial arguments for the Diffie-Hellman key exchange.
   * @param {number} p - Prime number.
   * @param {number} g - Prime number.
   * @returns {boolean} - Returns true if both p and g are prime, otherwise false.
   */
  static validateInitialArguments(p, g) {
    return isPrime(p) && isPrime(g);
  }
}

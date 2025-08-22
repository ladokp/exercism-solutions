/**
 * Validates a number using the Luhn algorithm.
 * 
 * The Luhn algorithm is used to validate various identification numbers, 
 * such as credit card numbers. This function will strip any spaces from 
 * the input number, double every second digit from the right, subtract 9 
 * from any result higher than 9, and then sum all the digits. If the total 
 * modulo 10 is zero and there is more than one digit, the number is valid.
 * 
 * @param {string} number - The number to validate, as a string.
 * @returns {boolean} - Returns true if the number is valid, false otherwise.
 */
export const valid = (number) => {
  // Remove all whitespace characters from the input number.
  const trimmed = number.replace(/\s/g, '');
  
  // Split the trimmed number into individual digits.
  const digits = [...trimmed];

  // Calculate the sum according to the Luhn algorithm.
  const sum = digits
    // Convert each character to an integer.
    .map((d) => parseInt(d, 10))
    // Double every second digit from the right.
    .map((d, i) => {
      if ((i + digits.length) % 2 === 0) {
        return d * 2;
      }
      return d;
    })
    // If doubling results in a number greater than 9, subtract 9.
    .map((d) => (d > 9 ? d - 9 : d))
    // Sum all the digits.
    .reduce((acc, d) => acc + d, 0);

  // The number is valid if the sum is divisible by 10 and there is more than one digit.
  return digits.length > 1 && sum % 10 === 0;
};

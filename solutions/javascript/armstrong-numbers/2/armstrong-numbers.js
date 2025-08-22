/**
 * Checks if a given number is an Armstrong number.
 *
 * An Armstrong number is a number that is equal to the sum of its own digits each raised to the power of the number of digits.
 *
 * @param {number|string} input - The number to check.
 * @returns {boolean} True if the input is an Armstrong number, false otherwise.
 */
export const isArmstrongNumber = (input) => {
  const bigInput = BigInt(input);

  // Convert the number to a string and split it into individual digits
  const digits = [...String(bigInput)];

  // Calculate the sum of each digit raised to the power of the number of digits
  const sum = digits.reduce(
    (total, current) => total + BigInt(current) ** BigInt(digits.length),
    BigInt(0)
  );

  // Check if the calculated sum is equal to the original number
  return sum === bigInput;
};

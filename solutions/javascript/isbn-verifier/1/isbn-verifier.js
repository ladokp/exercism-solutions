/**
 * Validates an ISBN-10 number.
 * An ISBN-10 is a 10-digit number where the last digit can be 'X' (representing 10).
 * The validation is based on the following criteria:
 * 1. The ISBN must be exactly 10 characters long.
 * 2. The first 9 characters must be digits.
 * 3. The last character can be either a digit or 'X'.
 * 4. The sum of the digits multiplied by their position (from 10 to 1) must be divisible by 11.
 * 
 * @param {string} isbn - The ISBN-10 number to validate.
 * @returns {boolean} - Returns true if the ISBN is valid, otherwise false.
 */
export const isValid = (isbn) => {
  // Remove hyphens from the ISBN.
  isbn = isbn.replace(/-/g, '');

  // Regular expression to match a valid ISBN-10 format.
  const isbnFormat = /^[0-9]{9}(X|\d)$/g;
  if (!isbnFormat.test(isbn)) {
    return false;
  }

  const isbnLength = 10;
  const sumOfProducts = [...isbn]
    .map((digit) => (digit === 'X' ? 10 : Number(digit))) // Convert 'X' to 10 and other characters to numbers.
    .map((digit, index) => digit * (isbnLength - index))  // Multiply each digit by its position.
    .reduce((sum, value) => sum + value, 0);              // Sum all the products.

  // An ISBN-10 is valid if the sum of the products is divisible by 11.
  return sumOfProducts % 11 === 0;
};

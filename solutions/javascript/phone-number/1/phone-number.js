/**
 * Cleans a given phone number by removing invalid characters and validating its format.
 *
 * @param {string} number - The phone number to clean.
 * @returns {string} - The cleaned phone number.
 * @throws {Error} - Throws an error if the phone number contains letters, punctuations, or has an invalid format.
 */
export const clean = (number) => {
  // Validate that the number does not contain letters
  if (/[a-zA-Z]/.test(number)) {
    throw new Error('Letters not permitted');
  }

  // Validate that the number does not contain specific punctuations
  if (/[@:!]/.test(number)) {
    throw new Error('Punctuations not permitted');
  }

  // Remove all non-digit characters
  let strippedNumber = number.replace(/\D/g, '');
  const numberLength = strippedNumber.length;

  // Validate the length of the number and adjust if it has 11 digits and starts with '1'
  if (numberLength === 11) {
    if (strippedNumber.charAt(0) !== '1') {
      throw new Error('11 digits must start with 1');
    }
    strippedNumber = strippedNumber.slice(1);
  }

  if (numberLength < 10) {
    throw new Error('Incorrect number of digits');
  }

  if (numberLength > 11) {
    throw new Error('More than 11 digits');
  }

  // Validate the area code and exchange code
  const areaCode = strippedNumber.charAt(0);
  const exchangeCode = strippedNumber.charAt(3);

  if (areaCode === '0') {
    throw new Error('Area code cannot start with zero');
  }
  if (areaCode === '1') {
    throw new Error('Area code cannot start with one');
  }
  if (exchangeCode === '0') {
    throw new Error('Exchange code cannot start with zero');
  }
  if (exchangeCode === '1') {
    throw new Error('Exchange code cannot start with one');
  }

  return strippedNumber;
};

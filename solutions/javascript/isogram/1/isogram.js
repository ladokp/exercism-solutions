/**
 * Checks if a given string is an isogram.
 * An isogram is a word or phrase without a repeating letter, 
 * ignoring spaces and hyphens.
 *
 * @param {string} string - The input string to be checked.
 * @returns {boolean} - Returns true if the string is an isogram, false otherwise.
 */
export const isIsogram = (string) => {
  // Remove spaces and hyphens from the string
  const stringNoSpaceOrHyphen = string.replace(/[\s-]/g, '');

  // Convert string to lowercase and split into individual characters
  const characters = stringNoSpaceOrHyphen.toLowerCase().split('');

  // Check if all characters are unique
  const uniqueCharacters = new Set(characters);

  // If the size of the set matches the length of the string, it's an isogram
  return uniqueCharacters.size === characters.length;
};
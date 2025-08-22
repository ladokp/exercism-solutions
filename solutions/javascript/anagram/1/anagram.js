/**
 * Normalize the string by converting it to lowercase, splitting it into characters, sorting them, and joining back.
 * @param {string} str - The string to normalize.
 * @returns {string} - The normalized string.
 */
function normalize(str) {
  return str.toLowerCase().split('').sort().join('');
}

/**
 * Check if two words are the same by comparing their lowercase forms.
 * @param {string} word - The original word.
 * @param {string} candidate - The candidate word to compare.
 * @returns {boolean} - True if the words are the same, false otherwise.
 */
function sameWord(word, candidate) {
  return word.toLowerCase() === candidate.toLowerCase();
}

/**
 * Check if the candidate word is an anagram of the original word.
 * @param {string} word - The original word.
 * @param {string} candidate - The candidate word to check.
 * @returns {boolean} - True if the candidate is an anagram, false otherwise.
 */
function isAnagram(word, candidate) {
  return normalize(word) === normalize(candidate);
}

/**
 * Find all anagrams of the subject word from the list of candidate words.
 * @param {string} subject - The subject word to find anagrams for.
 * @param {Array<string>} candidates - The list of candidate words.
 * @returns {Array<string>} - The list of anagrams.
 */
export function findAnagrams(subject, candidates) {
  const wordsCopy = Array.isArray(candidates) ? candidates : [...candidates];
  return wordsCopy.filter(
    (candidate) =>
      !sameWord(subject, candidate) && isAnagram(subject, candidate),
  );
}
/**
 * Counts the occurrences of each word in a given phrase.
 *
 * @param {string} phrase - The input phrase to count words from.
 * @returns {Object} An object where keys are words and values are the number of times each word appears in the phrase.
 */
export const countWords = (phrase) => {
  const wordCount = {};

  // Normalize the phrase by trimming, converting to lowercase and splitting into words
  const words = phrase.trim().toLowerCase().split(/[ ,\n]+/g);

  words.forEach((word) => {
    // Remove unwanted characters from each word
    const sanitizedWord = word.replace(/[.,!:"&@$%^]|^'|'$/g, '');

    if (sanitizedWord) {
      // Increment the word count or initialize it if it doesn't exist
      wordCount[sanitizedWord] = (wordCount[sanitizedWord] || 0) + 1;
    }
  });

  return wordCount;
};

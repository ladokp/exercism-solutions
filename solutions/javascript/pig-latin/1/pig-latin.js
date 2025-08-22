/**
 * Regular expression to match words starting with a vowel sound.
 * @type {RegExp}
 */
const VOWEL_SOUND_REGEXP = /^([aeiou]|xr|yt)/;

/**
 * Regular expression to match words starting with a consonant sound.
 * @type {RegExp}
 */
const CONSONANT_SOUND_REGEXP = /^([^aeiou]+(?=y)|[^aeiou]?qu|[^aeiou]+)([a-z]+)/;

/**
 * Translates a single word to Pig Latin.
 * If the word starts with a vowel sound, 'ay' is appended.
 * If the word starts with a consonant sound, the consonant(s) are moved to the end and 'ay' is appended.
 *
 * @param {string} word - The word to translate.
 * @returns {string} - The translated word in Pig Latin.
 */
function translateWord(word) {
  if (VOWEL_SOUND_REGEXP.test(word)) {
    return `${word}ay`;
  }

  const newWord = word.replace(CONSONANT_SOUND_REGEXP, '$2$1');
  return `${newWord}ay`;
}

/**
 * Translates a sentence to Pig Latin.
 *
 * @param {string} english - The sentence to translate.
 * @returns {string} - The translated sentence in Pig Latin.
 */
export const translate = (english) => {
  return english.split(' ').map(translateWord).join(' ');
};

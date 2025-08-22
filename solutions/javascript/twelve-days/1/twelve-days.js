// Ordinals for the verses
const ordinal = [
  null, 'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth'
];

// Gifts for each day
const gifts = [
  null, 'a Partridge in a Pear Tree.', 'two Turtle Doves', 'three French Hens', 
  'four Calling Birds', 'five Gold Rings', 'six Geese-a-Laying', 
  'seven Swans-a-Swimming', 'eight Maids-a-Milking', 'nine Ladies Dancing', 
  'ten Lords-a-Leaping', 'eleven Pipers Piping', 'twelve Drummers Drumming'
];

/**
 * Generate the prefix for the verse.
 * @param {number} verseNum - The verse number.
 * @returns {string} The prefix for the verse.
 */
const prefix = (verseNum) => `On the ${ordinal[verseNum]} day of Christmas my true love gave to me: `;

/**
 * Generate the gift list for the verse.
 * @param {number} verseNum - The verse number.
 * @returns {string} The gift list for the verse.
 */
const giftParade = (verseNum) => {
  const giftsList = [];
  for (let i = verseNum; i > 0; i--) {
    giftsList.push(gifts[i]);
  }
  if (verseNum > 1) {
    giftsList[giftsList.length - 1] = 'and ' + giftsList[giftsList.length - 1];
  }
  return giftsList.join(', ');
};

/**
 * Recite the verses of the song.
 * @param {number} startVerse - The starting verse number.
 * @param {number} [endVerse=startVerse] - The ending verse number.
 * @returns {string} The recited verses.
 */
export function recite(startVerse, endVerse = startVerse) {
  const verses = [];
  for (let i = startVerse; i <= endVerse; i++) {
    verses.push(`${prefix(i)}${giftParade(i)}\n`);
  }
  return verses.join('\n');
}

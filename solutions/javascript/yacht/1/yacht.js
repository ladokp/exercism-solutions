/**
 * Calculates the score for the Yacht category.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The score for the Yacht category.
 */
const getScoreForTheYachtCategory = (dices) => {
  const isYacht = new Set(dices).size === 1;
  return isYacht ? 50 : 0;
};

/**
 * Maps dice values to a counter array.
 * @param {number[]} dices - Array of dice values.
 * @returns {number[]} - Counter array for dice values.
 */
const mapDicesToCounterArray = (dices) => {
  let counterArray = [0, 0, 0, 0, 0, 0];
  for (let item of dices) {
    counterArray[item - 1]++;
  }
  return counterArray;
};

/**
 * Gets the number of appearances of a specific dice value.
 * @param {number[]} dices - Array of dice values.
 * @param {number} diceValue - The dice value to count.
 * @returns {number} - The number of appearances of the dice value.
 */
const getNoOfAppearances = (dices, diceValue) => {
  return dices.filter((value) => value === diceValue).length;
};

/**
 * Calculates the sum of dice values.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The sum of dice values.
 */
const getSumOfDices = (dices) => {
  return dices.reduce((a, b) => a + b, 0);
};

/**
 * Calculates the score for the Four of a Kind category.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The score for the Four of a Kind category.
 */
const getScoreForTheFourOfAKindCategory = (dices) => {
  const counterArray = mapDicesToCounterArray(dices);
  for (let i = 0; i < counterArray.length; i++) {
    if (counterArray[i] >= 4) {
      return 4 * (i + 1);
    }
  }
  return 0;
};

/**
 * Calculates the score for the Little Straight category.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The score for the Little Straight category.
 */
const getScoreForTheLittleStraightCategory = (dices) => {
  const counterArray = mapDicesToCounterArray(dices);
  const isLittleStraight = arrayIsFilledWithValue(
    counterArray,
    0,
    counterArray.length - 1,
    1,
  );
  return isLittleStraight ? 30 : 0;
};

/**
 * Calculates the score for the Big Straight category.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The score for the Big Straight category.
 */
const getScoreForTheBigStraightCategory = (dices) => {
  const counterArray = mapDicesToCounterArray(dices);
  const isBigStraight = arrayIsFilledWithValue(
    counterArray,
    1,
    counterArray.length,
    1,
  );
  return isBigStraight ? 30 : 0;
};

/**
 * Checks if an array is filled with a specific value within a range.
 * @param {number[]} array - Array to check.
 * @param {number} startPos - Start position in the array.
 * @param {number} endPos - End position in the array.
 * @param {number} value - Value to check for.
 * @returns {boolean} - True if the array is filled with the value, false otherwise.
 */
const arrayIsFilledWithValue = (array, startPos, endPos, value) => {
  for (let i = startPos; i < endPos; i++) {
    if (array[i] !== value) {
      return false;
    }
  }
  return true;
};

/**
 * Calculates the score for the Full House category.
 * @param {number[]} dices - Array of dice values.
 * @returns {number} - The score for the Full House category.
 */
const getScoreForTheFullHouseCategory = (dices) => {
  const counterArray = mapDicesToCounterArray(dices);
  let hasTwoOfAKind = false;
  let hasThreeOfAKind = false;
  for (let item of counterArray) {
    if (item === 2) {
      hasTwoOfAKind = true;
    } else if (item === 3) {
      hasThreeOfAKind = true;
    }
  }
  return hasTwoOfAKind && hasThreeOfAKind ? getSumOfDices(dices) : 0;
};

/**
 * Calculates the score based on the dice values and the category.
 * @param {number[]} dices - Array of dice values.
 * @param {string} category - Category to calculate the score for.
 * @returns {number} - The calculated score.
 */
export const score = (dices, category) => {
  switch (category) {
    case 'yacht':
      return getScoreForTheYachtCategory(dices);
    case 'ones':
      return getNoOfAppearances(dices, 1);
    case 'twos':
      return 2 * getNoOfAppearances(dices, 2);
    case 'threes':
      return 3 * getNoOfAppearances(dices, 3);
    case 'fours':
      return 4 * getNoOfAppearances(dices, 4);
    case 'fives':
      return 5 * getNoOfAppearances(dices, 5);
    case 'sixes':
      return 6 * getNoOfAppearances(dices, 6);
    case 'full house':
      return getScoreForTheFullHouseCategory(dices);
    case 'four of a kind':
      return getScoreForTheFourOfAKindCategory(dices);
    case 'little straight':
      return getScoreForTheLittleStraightCategory(dices);
    case 'big straight':
      return getScoreForTheBigStraightCategory(dices);
    case 'choice':
      return getSumOfDices(dices);
  }
  return 0;
};

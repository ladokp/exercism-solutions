/* eslint-disable no-underscore-dangle */

/**
 * This module generates unique robot names in the format:
 * Two uppercase letters followed by three digits (e.g., 'AB123').
 * The names are shuffled to provide randomness and uniqueness.
 * Once all names are used, generating additional names will throw an error.
 */

/** Array of uppercase English letters */
const LETTERS = [...'QWERTYUIOPASDFGHJKLZXCVBNM'];

/** Array of digits (0 to 9) */
const NUMBERS = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

/** Pre-generate all possible unique robot names */
const ALL_NAMES = LETTERS.flatMap((firstLetter) =>
  LETTERS.flatMap((secondLetter) =>
    NUMBERS.flatMap((firstDigit) =>
      NUMBERS.flatMap((secondDigit) =>
        NUMBERS.map((thirdDigit) =>
          `${firstLetter}${secondLetter}${firstDigit}${secondDigit}${thirdDigit}`
        )
      )
    )
  )
);

/** Array to hold the shuffled robot names */
const shuffled = [...ALL_NAMES];
let shuffledPointer = -1;

/**
 * Shuffles the robot names using the Fisher-Yates algorithm.
 * This ensures the names are randomized and no duplicates occur.
 */
function shuffleNames() {
  for (let i = shuffled.length - 1; i > 0; i -= 1) {
    const j = Math.floor(Math.random() * (i + 1));
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
}

/**
 * Generates the next available unique robot name.
 * @throws {Error} If all possible names have been used.
 * @returns {string} The next unique robot name.
 */
function generateName() {
  shuffledPointer += 1;
  if (shuffledPointer >= shuffled.length) {
    throw new Error('Cannot generate another name because all names have been used.');
  }
  return shuffled[shuffledPointer];
}

// Initial shuffle to randomize the names
shuffleNames();

/**
 * Represents a Robot with a unique name.
 */
export class Robot {
  constructor() {
    this._name = generateName();
  }

  /**
   * Gets the robot's name.
   * @returns {string} The current name of the robot.
   */
  get name() {
    return this._name;
  }

  /**
   * Resets the robot's name to a new unique name.
   */
  reset() {
    this._name = generateName();
  }
}

/**
 * Resets the internal pointer to allow reusing all names.
 * Useful for resetting the name pool.
 */
Robot.releaseNames = () => {
  shuffledPointer = -1;
};

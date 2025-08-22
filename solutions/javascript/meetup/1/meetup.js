/**
 * Array representing the days of the week.
 * @type {string[]}
 */
const weekday = [
  'Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday',
];

/**
 * Get the number of days in a given month of a year.
 * @param {number} year - The year.
 * @param {number} month - The month (1-12).
 * @returns {number} The number of days in the month.
 */
const daysInMonth = (year, month) => new Date(year, month, 0).getDate();

/**
 * Get the index of the day of the week.
 * @param {string} dayOfWeek - The day of the week (e.g., 'Monday').
 * @returns {number} The index of the day in the `weekday` array.
 */
const getDayOfWeekCount = (dayOfWeek) => weekday.indexOf(dayOfWeek);

/**
 * Get the meetup day for a given year, month, week ordinal, and day of the week.
 * @param {number} year - The year.
 * @param {number} month - The month (1-12).
 * @param {string} weekOrdinal - The week ordinal (e.g., 'first', 'second', 'teenth').
 * @param {string} dayOfWeek - The day of the week (e.g., 'Monday').
 * @returns {number} The day of the month that matches the meetup criteria.
 * @throws Will throw an error if no matching day is found.
 */
const getMeetupDay = (year, month, weekOrdinal, dayOfWeek) => {
  let lowerLimit = 1;
  let upperLimit = daysInMonth(year, month);
  switch (weekOrdinal) {
    case 'first':
      upperLimit = 7;
      break;
    case 'second':
      lowerLimit = 8;
      upperLimit = 14;
      break;
    case 'third':
      lowerLimit = 15;
      upperLimit = 21;
      break;
    case 'fourth':
      lowerLimit = 22;
      upperLimit = 28;
      break;
    case 'last':
      lowerLimit = upperLimit - 6;
      break;
    case 'teenth':
      lowerLimit = 13;
      upperLimit = 19;
      break;
    default:
      throw new Error(`Invalid week ordinal: ${weekOrdinal}`);
  }

  for (let i = lowerLimit; i <= upperLimit; i++) {
    if (new Date(year, month - 1, i).getDay() === getDayOfWeekCount(dayOfWeek)) {
      return i;
    }
  }
  throw new Error(`No matching day found for ${weekOrdinal} ${dayOfWeek} in ${month}/${year}`);
};

/**
 * Get the meetup date for a given year, month, week ordinal, and day of the week.
 * @param {number} year - The year.
 * @param {number} month - The month (1-12).
 * @param {string} weekOrdinal - The week ordinal (e.g., 'first', 'second', 'teenth').
 * @param {string} dayOfWeek - The day of the week (e.g., 'Monday').
 * @returns {Date} The meetup date.
 */
export const meetup = (year, month, weekOrdinal, dayOfWeek) => {
  const meetupDay = getMeetupDay(year, month, weekOrdinal, dayOfWeek);
  return new Date(year, month - 1, meetupDay);
};

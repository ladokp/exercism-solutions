// @ts-check

/**
 * Given a certain command, help the chatbot recognize whether the command is valid or not.
 *
 * @param {string} command
 * @returns {boolean} whether or not is the command valid
 */
export function isValidCommand(command) {
  const commandRegex = /^chatbot/i;
  return commandRegex.test(command);
}

/**
 * Given a certain message, help the chatbot get rid of all the emoji's encryption throughout the message.
 *
 * @param {string} message
 * @returns {string} The message without the emojis encryption
 */
export function removeEmoji(message) {
  const emojiRegex = /emoji[0-9]+/g;
  return message.replace(emojiRegex, '');
}

/**
 * Given a certain phone number, help the chatbot recognize whether it is in the correct format.
 *
 * @param {string} number
 * @returns {string} the Chatbot response to the phone Validation
 */
export function checkPhoneNumber(number) {
  const phoneRegex = /^\(\+?([0-9]{2})\)?[-. ]?([0-9]{3})[-. ]?([0-9]{3})[-. ]?([0-9]{3})$/;
  if (phoneRegex.test(number)) {
    return 'Thanks! You can now download me to your phone.';
  } else {
    return `Oops, it seems like I can't reach out to ${number}`;
  }
}

/**
 * Given a certain response from the user, help the chatbot get only the URL.
 *
 * @param {string} userInput
 * @returns {string[] | null} all the possible URL's that the user may have answered
 */
export function getURL(userInput) {
  const urlRegex = /(\w+\.)+\w+/g;
  return userInput.match(urlRegex);
}

/**
 * Greet the user using its full name data from the profile.
 *
 * @param {string} fullName
 * @returns {string} Greeting from the chatbot
 */
export function niceToMeetYou(fullName) {
  return fullName.replace(/(?<surname>\w+), (?<name>\w+)/, (_, surname, name) => {
    return `Nice to meet you, ${name} ${surname}`;
  });
}

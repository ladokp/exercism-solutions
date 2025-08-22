/**
 * The alphabet used for encoding and decoding.
 * @constant {string}
 */
const ALPHABET = 'abcdefghijklmnopqrstuvwxyz';

/**
 * Encodes a given message using the Atbash cipher.
 * Removes spaces, periods, and commas, and converts to lowercase.
 * Groups the encoded message into chunks of five characters.
 * 
 * @param {string} message - The message to encode.
 * @returns {string} The encoded message.
 */
export const encode = (message) => {
  // Remove spaces, periods, and commas, and convert to lowercase
  const sanitizedMessage = message.toLowerCase().replace(/[ .,]/g, '');
  
  const encodedMessage = [...sanitizedMessage]
    .map((char) => {
      if (ALPHABET.includes(char)) {
        return ALPHABET[ALPHABET.length - 1 - ALPHABET.indexOf(char)];
      }
      return char;
    })
    .join('');
  
  // Group the encoded message into chunks of five characters
  return encodedMessage.match(/.{1,5}/g).join(' ');
};

/**
 * Decodes a given message using the Atbash cipher.
 * Removes spaces from the encoded message.
 * 
 * @param {string} message - The message to decode.
 * @returns {string} The decoded message.
 */
export const decode = (message) => {
  return encode(message).replace(/ /g, '');
};

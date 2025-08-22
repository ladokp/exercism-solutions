const LENGTH = 7;
const CONT_BITS = 1 << LENGTH;
const DATA_BITS = CONT_BITS - 1;

/**
 * Encodes a single value using variable-length quantity encoding.
 *
 * @param {number} val - The value to encode.
 * @returns {number[]} - The encoded value as an array of bytes.
 */
const encodeOne = (val) => {
  const buf = [];
  let left = val;

  while (left) {
    const bits = (left & DATA_BITS) | CONT_BITS; // set continuation everywhere
    left = left >>> LENGTH;
    buf.push(bits);
  }
  buf[0] = buf[0] & DATA_BITS; // cancel the last continuation
  return buf.reverse();
};

/**
 * Decodes a single value from variable-length quantity encoding.
 *
 * @param {number[]} buf - The encoded value as an array of bytes.
 * @returns {number} - The decoded value.
 */
const decodeOne = (buf) => {
  let val = 0;

  for (const byte of buf) {
    val = (val << LENGTH) | (byte & DATA_BITS);
  }
  return val >>> 0; // convert to unsigned 32-bit
};

/**
 * Encodes an array of values using variable-length quantity encoding.
 *
 * @param {number[]} data - The array of values to encode.
 * @returns {number[]} - The encoded values as an array of bytes.
 */
export const encode = (data) => {
  return data.reduce((buf, val) => buf.concat(encodeOne(val)), []);
};

/**
 * Decodes an array of bytes from variable-length quantity encoding.
 *
 * @param {number[]} data - The encoded values as an array of bytes.
 * @returns {number[]} - The decoded values.
 * @throws {Error} - If the sequence is incomplete.
 */
export const decode = (data) => {
  let start = 0;
  const vals = [];

  for (let i = 0; i < data.length; i++) {
    if (~data[i] & CONT_BITS) {
      vals.push(decodeOne(data.slice(start, i + 1)));
      start = i + 1;
    }
  }
  if (start < data.length) {
    throw new Error('Incomplete sequence');
  }
  return vals;
};

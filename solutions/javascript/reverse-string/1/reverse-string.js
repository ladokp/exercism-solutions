//
// This is only a SKELETON file for the 'Reverse String' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export const reverseString = (str) => {
  let result = "";
  for (let character of str) {
    result = character + result;
  }
  return result;
};

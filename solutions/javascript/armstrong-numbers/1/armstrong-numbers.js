//
// This is only a SKELETON file for the 'Armstrong Numbers' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

export function isArmstrongNumber(input) {
  const digits = [...String(input)];
  const sum = digits.reduce(
    (total, current) => total + current ** digits.length,
    0,
  );
  return sum === input;
};

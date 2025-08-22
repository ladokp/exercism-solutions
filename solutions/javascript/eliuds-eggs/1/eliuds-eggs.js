export const eggCount = (displayValue) => {
  if (typeof displayValue !== 'number' || displayValue < 0 || !Number.isInteger(displayValue)) {
    throw new Error('Input must be a non-negative integer.');
  }

  let count = 0;
  while (displayValue > 0) {
    if (displayValue % 2 === 1) {
      count++;
    }
    displayValue = Math.floor(displayValue / 2);
  }
  return count;
};

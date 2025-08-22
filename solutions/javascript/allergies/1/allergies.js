const possibleAllergies = [
  'eggs',
  'peanuts',
  'shellfish',
  'strawberries',
  'tomatoes',
  'chocolate',
  'pollen',
  'cats',
];

export class Allergies {
  constructor(allergenIndex) {
    this.allergenIndex = allergenIndex;
    // eslint-disable-next-line no-bitwise, no-restricted-properties
    this.allergies = possibleAllergies.filter(
      (allergy, index) => this.allergenIndex & Math.pow(2, index),
    );
  }

  list() {
    return this.allergies;
  }

  allergicTo(food) {
    return this.allergies.some((allergy) => allergy === food);
  }
}
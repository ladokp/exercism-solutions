export class Squares {
  constructor(naturalNumbers) {
    this.naturalNumbers = naturalNumbers;
  }

  get sumOfSquares() {
    return this.naturalNumbers * (this.naturalNumbers + 1) * (2 * this.naturalNumbers + 1) / 6;
  }

  get squareOfSum() {
    return Math.pow((this.naturalNumbers * (this.naturalNumbers + 1) / 2), 2);
  }

  get difference() {
    return this.squareOfSum - this.sumOfSquares;
  }
}

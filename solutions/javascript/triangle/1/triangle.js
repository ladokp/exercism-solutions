export class Triangle {
  constructor(...sides) {
    this.sidesSet = new Set(sides);
    this.isValid = eval(sides.join('+')) > (2 * Math.max(...sides));
  }
  
  get isEquilateral() {
    return this.isValid && this.sidesSet.size == 1;
  }

  get isIsosceles() {
    return this.isValid && this.sidesSet.size < 3;
  }

  get isScalene() {
    return this.isValid && this.sidesSet.size == 3;
  }
}

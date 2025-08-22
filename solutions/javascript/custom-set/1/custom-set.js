export class CustomSet {
  /**
   * Creates a custom set.
   * @param {number[]} elements - An optional list of initial values.
   */
  constructor(elements = []) {
    this.elements = [];
    elements.every(element => this.add(element));
  }

  /**
   * Checks if the set is empty.
   * @returns {boolean} - `true` if the set is empty, otherwise `false`.
   */
  empty() {
    return this.elements.length === 0;
  }

  /**
   * Checks if the set contains a specific element.
   * @param {number} element - The element to check.
   * @returns {boolean} - `true` if the element is present, otherwise `false`.
   */
  contains(element) {
    return this.elements.includes(element);
  }

  /**
   * Adds an element to the set if it is not already present.
   * @param {number} element - The element to add.
   * @returns {CustomSet} - The updated set (allows chaining).
   */
  add(element) {
    if (!this.contains(element)) {
      this.elements.push(element);
    }
    return this;
  }

  /**
   * Checks if the current set is a subset of another set.
   * @param {CustomSet} otherSet - The other set.
   * @returns {boolean} - `true` if all elements of the current set exist in `otherSet`.
   */
  subset(otherSet) {
    return this.elements.every(element => otherSet.contains(element));
  }

  /**
   * Checks if the set has no common elements with another set.
   * @param {CustomSet} otherSet - The other set.
   * @returns {boolean} - `true` if there are no shared elements.
   */
  disjoint(otherSet) {
    return this.elements.every(element => !otherSet.contains(element));
  }

  /**
   * Checks if two sets are equal.
   * @param {CustomSet} otherSet - The other set.
   * @returns {boolean} - `true` if both sets contain the same elements.
   */
  eql(otherSet) {
    return this.elements.length === otherSet.elements.length && this.subset(otherSet);
  }

  /**
   * Returns a new set containing all elements from both sets.
   * @param {CustomSet} otherSet - The other set.
   * @returns {CustomSet} - A new set representing the union.
   */
  union(otherSet) {
    return new CustomSet([...this.elements, ...otherSet.elements]);
  }

  /**
   * Returns a new set containing only the common elements of both sets.
   * @param {CustomSet} otherSet - The other set.
   * @returns {CustomSet} - A new set representing the intersection.
   */
  intersection(otherSet) {
    return new CustomSet(this.elements.filter(element => otherSet.contains(element)));
  }

  /**
   * Returns a new set containing elements that are in the current set but not in `otherSet`.
   * @param {CustomSet} otherSet - The other set.
   * @returns {CustomSet} - A new set representing the difference.
   */
  difference(otherSet) {
    return new CustomSet(this.elements.filter(element => !otherSet.contains(element)));
  }
}

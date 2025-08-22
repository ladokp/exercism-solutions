/**
 * Class representing a list.
 */
export class List {
  /**
   * Create a list.
   * @param {Array} list - The list of elements.
   */
  constructor(list = []) {
    this.list = list;
  }

  /**
   * Compare this list with another list.
   * @param {List} other - The other list to compare with.
   * @returns {string} The comparison result ('SUBLIST', 'EQUAL', 'SUPERLIST', or 'UNEQUAL').
   */
  compare(other) {
    const diff = lengthDiff(this, other);
    if (diff === '-1' && isSublist(other.list, this.list)) return 'SUBLIST';
    if (diff === '0' && isSublist(other.list, this.list)) return 'EQUAL';
    if (diff === '1' && isSublist(this.list, other.list)) return 'SUPERLIST';
    return 'UNEQUAL';
  }
}

/**
 * Calculate the length difference between two lists.
 * @param {List} one - The first list.
 * @param {List} two - The second list.
 * @returns {string} The length difference as a string ('-1', '0', or '1').
 */
function lengthDiff(one, two) {
  return String(Math.sign(one.list.length - two.list.length));
}

/**
 * Check if one list is a sublist of another list.
 * @param {Array} one - The list to check against.
 * @param {Array} two - The potential sublist.
 * @returns {boolean} True if 'two' is a sublist of 'one', otherwise false.
 */
function isSublist(one, two) {
  return one.join().includes(two.join());
}

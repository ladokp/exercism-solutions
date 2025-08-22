/**
 * Represents an empty node in the list (similar to a `null` value).
 * Implements methods that return default values or the Null object itself.
 */
const Null = {
  /** Returns the value of Null, which is undefined. */
  get value() {
    return undefined;
  },

  /** The next node of Null is itself. */
  get next() {
    return this;
  },

  /** Returns an empty array, representing no values. */
  get values() {
    return [];
  },

  /** Returns undefined when trying to get a value from Null. */
  get() {
    return this.value;
  },

  /**
   * Creates a new list with the given item prepended.
   * @param {*} item - The item to add to the list.
   * @returns {Cons} A new list containing the item.
   */
  push(item) {
    return new Cons(item, this);
  },

  /** Returns 0 as the length of Null. */
  length() {
    return 0;
  },

  /**
   * Appends another list to Null, which simply returns the other list.
   * @param {Cons|Null} other - The list to append.
   * @returns {Cons|Null} The other list.
   */
  append(other) {
    return other;
  },

  /** Returns Null when concatenating with other lists. */
  concat() {
    return this;
  },

  /** Does nothing when iterating over Null. */
  forEach() {
    /* done */
  },

  /**
   * Folds the list from the left with an initial value.
   * @param {Function} _ - The callback function (unused).
   * @param {*} initial - The initial accumulator value.
   * @returns {*} The initial value.
   */
  foldl(_, initial) {
    return initial;
  },

  /**
   * Folds the list from the right with an initial value.
   * @param {Function} _ - The callback function (unused).
   * @param {*} initial - The initial accumulator value.
   * @returns {*} The initial value.
   */
  foldr(_, initial) {
    return initial;
  },

  /** Returns Null when filtering. */
  filter() {
    return Null;
  },

  /** Returns Null when reversing. */
  reverse() {
    return this;
  },

  /** Returns Null when mapping. */
  map() {
    return this;
  },
};

/**
 * Represents a non-empty list node with a value and a reference to the next node.
 */
class Cons {
  /**
   * Creates a list from an array.
   * @param {Array} arr - The array to convert into a list.
   * @returns {Cons|Null} A list representing the array.
   */
  static fromArray(arr) {
    let list = Null;
    for (let i = arr.length - 1; i >= 0; i--) {
      list = new Cons(arr[i], list);
    }
    return list;
  }

  /**
   * Constructs a list node with a value and a next node.
   * @param {*} value - The value of the node.
   * @param {Cons|Null} next - The next node in the list.
   */
  constructor(value, next = Null) {
    this.value = value;
    this.next = next;
  }

  /** Returns all values in the list as an array. */
  get values() {
    const result = [];
    this.forEach((item) => result.push(item));
    return result;
  }

  /**
   * Gets the value at the specified index.
   * @param {number} i - The index to retrieve.
   * @returns {*} The value at the index, or undefined if out of bounds.
   */
  get(i) {
    let current = this;
    let index = i;
    while (current !== Null) {
      if (index === 0) return current.value;
      current = current.next;
      index--;
    }
    return undefined;
  }

  /**
   * Creates a new list with the item prepended.
   * @param {*} item - The item to add to the list.
   * @returns {Cons} A new list with the item added.
   */
  push(item) {
    return new Cons(item, this);
  }

  /** Returns the length of the list. */
  length() {
    let current = this;
    let count = 0;
    while (current !== Null) {
      count++;
      current = current.next;
    }
    return count;
  }

  /**
   * Appends another list to the current list.
   * @param {Cons|Null} other - The list to append.
   * @returns {Cons} A new list with the other list appended.
   */
  append(other) {
    return this.foldr((acc, item) => new Cons(item, acc), other);
  }

  /**
   * Concatenates a list of lists.
   * @param {Cons|Null} others - The list of lists to concatenate.
   * @returns {Cons} A new list with all lists concatenated.
   */
  concat(others) {
    return others.foldl((result, other) => result.append(other), this);
  }

  /**
   * Folds the list from the left.
   * @param {Function} callback - The function to apply.
   * @param {*} initial - The initial accumulator value.
   * @returns {*} The final accumulated value.
   */
  foldl(callback, initial) {
    let current = this;
    let acc = initial;
    while (current !== Null) {
      acc = callback(acc, current.value);
      current = current.next;
    }
    return acc;
  }

  /**
   * Iterates over each item in the list.
   * @param {Function} callback - The function to execute for each item.
   */
  forEach(callback) {
    let current = this;
    while (current !== Null) {
      callback(current.value);
      current = current.next;
    }
  }

  /**
   * Folds the list from the right.
   * @param {Function} callback - The function to apply.
   * @param {*} initial - The initial accumulator value.
   * @returns {*} The final accumulated value.
   */
  foldr(callback, initial) {
    return callback(this.next.foldr(callback, initial), this.value);
  }

  /**
   * Filters the list based on a predicate function.
   * @param {Function} predicate - The function to test each item.
   * @returns {Cons|Null} A new list with items that pass the predicate.
   */
  filter(predicate) {
    return this.foldr((acc, item) => (predicate(item) ? new Cons(item, acc) : acc), Null);
  }

  /**
   * Maps each item to a new value using the provided function.
   * @param {Function} expression - The function to apply to each item.
   * @returns {Cons|Null} A new list with the mapped values.
   */
  map(expression) {
    return this.foldr((acc, item) => new Cons(expression(item), acc), Null);
  }

  /**
   * Reverses the list.
   * @returns {Cons|Null} A new list with the items reversed.
   */
  reverse() {
    let result = Null;
    let current = this;
    while (current !== Null) {
      result = new Cons(current.value, result);
      current = current.next;
    }
    return result;
  }
}

/**
 * Represents a list that can be initialized with an array of values.
 */
export class List {
  /**
   * Constructs a new list from an array.
   * @param {Array} values - The array to convert into a list.
   * @returns {Cons|Null} A list representing the array.
   */
  constructor(values = []) {
    return Cons.fromArray(values);
  }
}

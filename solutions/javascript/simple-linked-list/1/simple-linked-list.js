/**
 * Represents an element in the linked list.
 */
export class Element {
  /**
   * Creates an instance of Element.
   * @param {*} value - The value of the element.
   */
  constructor(value) {
    this.value = value;
    this.next = null;
  }
}

/**
 * Represents a linked list.
 */
export class List {
  /**
   * Creates an instance of List.
   * @param {Array} [arr] - An optional array to initialize the list.
   */
  constructor(arr) {
    this.head = null;
    if (arr) {
      arr.forEach((el) => {
        this.add(new Element(el));
      });
    }
  }

  /**
   * Gets the length of the linked list.
   * @return {number} The number of elements in the list.
   */
  get length() {
    return this.head ? this.countElements(1, this.head) : 0;
  }

  /**
   * Adds a new element to the front of the list.
   * @param {Element} el - The element to add.
   */
  add(el) {
    const element = el;
    element.next = this.head;
    this.head = element;
  }

  /**
   * Recursively counts the number of elements in the list.
   * @param {number} count - The current count of elements.
   * @param {Element} element - The current element.
   * @return {number} The total number of elements.
   */
  countElements(count, element) {
    return element.next ? this.countElements(count + 1, element.next) : count;
  }

  /**
   * Converts the linked list to an array.
   * @param {Array} [arr=[]] - The array to store the elements.
   * @param {Element} [element=this.head] - The current element.
   * @return {Array} The array containing all elements of the list.
   */
  toArray(arr = [], element = this.head) {
    arr.push(element.value);
    return element && element.next ? this.toArray(arr, element.next) : arr;
  }

  /**
   * Reverses the linked list.
   * @param {Element} [prev=null] - The previous element.
   * @return {List} The reversed list.
   */
  reverse(prev = null) {
    if (this.head.next) {
      const current = this.head;
      this.head = this.head.next;
      current.next = prev;
      return this.reverse(current);
    }
    this.head.next = prev;
    return this;
  }
}

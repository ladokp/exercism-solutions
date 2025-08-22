class Node {
  constructor({ value, next = null, prev = null }) {
    this.value = value;
    this.next = next;
    this.prev = prev;
  }
}

export class LinkedList {
  constructor() {
    this.head = null;
    this.tail = null;
    this.length = 0;
  }

  clear() {
    this.head = null;
    this.tail = null;
    this.length = 0;
  }

  pop() {
    if (!this.tail) {
      throw new Error('The list is empty');
    }

    const { value } = this.tail;

    if (this.tail.prev) {
      this.tail = this.tail.prev;
      this.tail.next = null;
    } else {
      this.clear();
    }

    this.length = Math.max(0, this.length - 1); // Ensure length is not negative
    return value;
  }

  push(value) {
    const element = new Node({ value, prev: this.tail });

    if (this.tail) {
      this.tail.next = element;
    } else {
      this.head = element;
    }

    this.tail = element;
    this.length += 1;
  }

  shift() {
    if (!this.head) {
      throw new Error('The list is empty');
    }

    const { value } = this.head;

    if (this.head.next) {
      this.head = this.head.next;
      this.head.prev = null;
    } else {
      this.clear();
    }

    this.length = Math.max(0, this.length - 1); // Ensure length is not negative
    return value;
  }

  unshift(value) {
    const element = new Node({ value, next: this.head });

    if (this.head) {
      this.head.prev = element;
    } else {
      this.tail = element;
    }

    this.head = element;
    this.length += 1;
  }

  count() {
    return this.length;
  }

  delete(value) {
    let element = this.head;

    while (element) {
      if (element.value === value) {
        if (element.prev) {
          element.prev.next = element.next;
        } else {
          this.head = element.next;
        }

        if (element.next) {
          element.next.prev = element.prev;
        } else {
          this.tail = element.prev;
        }

        this.length = Math.max(0, this.length - 1);
        return;
      }

      element = element.next;
    }
  }
}

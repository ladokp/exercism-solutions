/**
 * Represents a node in a Binary Search Tree.
 */
export class BinarySearchTree {
  /**
   * Creates an instance of BinarySearchTree.
   * @param {number} data - The value of the node.
   */
  constructor(data) {
    this.data = data;
    this.left = undefined;
    this.right = undefined;
  }

  /**
   * Inserts a value into the Binary Search Tree.
   * @param {number} value - The value to be inserted.
   * @returns {BinarySearchTree} The current node after insertion.
   */
  insert(value) {
    if (value <= this.data) {
      this.insertLeft(value);
    } else {
      this.insertRight(value);
    }
    return this;
  }

  /**
   * Inserts a value into the left subtree.
   * @param {number} value - The value to be inserted.
   * @returns {BinarySearchTree} The current node after insertion.
   */
  insertLeft(value) {
    if (!this.left) {
      this.left = new BinarySearchTree(value);
    } else {
      this.left.insert(value);
    }
    return this;
  }

  /**
   * Inserts a value into the right subtree.
   * @param {number} value - The value to be inserted.
   * @returns {BinarySearchTree} The current node after insertion.
   */
  insertRight(value) {
    if (!this.right) {
      this.right = new BinarySearchTree(value);
    } else {
      this.right.insert(value);
    }
    return this;
  }

  /**
   * Traverses the tree and executes a function on each node's data.
   * @param {function} fn - The function to execute on each node's data.
   */
  each(fn) {
    if (this.left) {
      this.left.each(fn);
    }
    fn.call(this, this.data);
    if (this.right) {
      this.right.each(fn);
    }
  }
}

/**
 * Represents a bank account with basic operations such as opening, closing,
 * depositing, and withdrawing funds.
 */
export class BankAccount {
  /**
   * Opens the bank account. Throws an error if the account is already open.
   */
  open() {
    if (this.isOpen) {
      throw new ValueError('Account is already open.');
    }
    this._open = true;
    this._balance = 0;
  }

  /**
   * Closes the bank account. Throws an error if the account is not open.
   */
  close() {
    this.verifyAccount();
    this._open = false;
  }

  /**
   * Verifies that the account is open. Throws an error if the account is not open.
   * @private
   */
  verifyAccount() {
    if (!this.isOpen) {
      throw new ValueError('Account is not open.');
    }
  }

  /**
   * Deposits the specified amount into the account. Throws an error if the amount
   * is negative or if the account is not open.
   * @param {number} amount - The amount to deposit.
   */
  deposit(amount) {
    this.verifyAccount();
    this.verifyDeposit(amount);
    this._balance += amount;
  }

  /**
   * Verifies that the deposit amount is valid. Throws an error if the amount
   * is negative.
   * @param {number} amount - The amount to verify.
   * @private
   */
  verifyDeposit(amount) {
    if (amount < 0) {
      throw new ValueError('Deposit amount must be non-negative.');
    }
  }

  /**
   * Withdraws the specified amount from the account. Throws an error if the amount
   * is negative, exceeds the current balance, or if the account is not open.
   * @param {number} amount - The amount to withdraw.
   */
  withdraw(amount) {
    this.verifyAccount();
    this.verifyWithdraw(amount);
    this._balance -= amount;
  }

  /**
   * Verifies that the withdrawal amount is valid. Throws an error if the amount
   * is negative or exceeds the current balance.
   * @param {number} amount - The amount to verify.
   * @private
   */
  verifyWithdraw(amount) {
    if (amount < 0 || amount > this._balance) {
      throw new ValueError('Invalid withdrawal amount.');
    }
  }

  /**
   * Gets the current balance of the account. Throws an error if the account is not open.
   * @returns {number} The current balance.
   */
  get balance() {
    this.verifyAccount();
    return this._balance;
  }

  /**
   * Checks if the account is open.
   * @returns {boolean} True if the account is open, false otherwise.
   */
  get isOpen() {
    return this._open;
  }
}

/**
 * Represents a value error, typically used for invalid operations or inputs.
 */
export class ValueError extends Error {}
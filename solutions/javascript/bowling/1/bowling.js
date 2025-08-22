/**
 * Class representing a Bowling game.
 */
export class Bowling {
  constructor() {
    this.maxPins = 10;
    this.maxFrames = 10;
    this.currentFrame = 0;

    this.frames = [];
    this.frameScores = Array(this.maxFrames).fill(0);

    this.initializeFrame();
  }

  /**
   * Initialize a new frame.
   */
  initializeFrame() {
    this.frameRoll = 1;
    this.remainingPins = this.maxPins;
    this.currentFrame += 1;
  }

  /**
   * Increment the frame roll.
   */
  incrementFrame() {
    this.frameRoll += 1;
  }

  /**
   * Increment the score for the current frame.
   * @param {number} pins - The number of pins knocked down.
   */
  incrementScore(pins) {
    if (this.currentFrame > this.maxFrames) return;
    this.frameScores[this.currentFrame - 1] += pins;
  }

  /**
   * Score a strike.
   */
  scoreStrike() {
    this.frames[this.currentFrame - 1] = 'X';
    this.applyStrikeBonus(this.maxPins);
    this.applySpareBonus(this.maxPins);
    this.incrementFrame();
  }

  /**
   * Score the first roll of a frame.
   * @param {number} pins - The number of pins knocked down.
   */
  scoreFirstRoll(pins) {
    this.remainingPins -= pins;
    this.applySpareBonus(pins);
    this.applyStrikeBonus(pins);
    this.incrementFrame();
  }

  /**
   * Score a spare.
   * @param {number} pins - The number of pins knocked down.
   */
  scoreSpare(pins) {
    this.frames[this.currentFrame - 1] = 'S';
    this.applyStrikeBonus(pins);
    this.incrementFrame();
  }

  /**
   * Score an open frame.
   * @param {number} pins - The number of pins knocked down.
   */
  scoreOpenFrame(pins) {
    this.frames[this.currentFrame - 1] = this.maxPins - this.remainingPins + pins;
    this.applyStrikeBonus(pins);
    this.incrementFrame();
  }

  /**
   * Apply the spare bonus to the score.
   * @param {number} pins - The number of pins knocked down.
   */
  applySpareBonus(pins) {
    if (this.frames[this.currentFrame - 2] === 'S') {
      this.frameScores[this.currentFrame - 2] += pins;
    }
  }

  /**
   * Apply the strike bonus to the score.
   * @param {number} pins - The number of pins knocked down.
   */
  applyStrikeBonus(pins) {
    if (
      this.frames[this.currentFrame - 3] === 'X' &&
      this.frames[this.currentFrame - 2] === 'X' &&
      this.frameRoll === 1 &&
      this.currentFrame <= this.maxFrames + 2
    ) {
      this.frameScores[this.currentFrame - 3] += pins;
    }
    if (
      this.frames[this.currentFrame - 2] === 'X' &&
      this.currentFrame <= this.maxFrames + 1
    ) {
      this.frameScores[this.currentFrame - 2] += pins;
    }
  }

  /**
   * Check if the game is over.
   * @returns {boolean} - True if the game is over, false otherwise.
   */
  isGameOver() {
    if (this.currentFrame <= this.maxFrames) return false;

    if (
      this.frames[this.maxFrames - 1] !== 'X' &&
      this.frames[this.maxFrames - 1] !== 'S'
    )
      return true;

    if (this.frames[this.maxFrames - 1] === 'S' && this.frameRoll > 1)
      return true;

    if (
      this.frames[this.maxFrames - 1] === 'S' &&
      this.frames[this.maxFrames] === 'X'
    )
      return true;

    if (this.frames[this.maxFrames - 1] === 'X') {
      if (
        this.frames[this.maxFrames] !== 'X' &&
        this.currentFrame > this.maxFrames + 1
      )
        return true;

      if (this.frames[this.maxFrames] === 'X') {
        if (this.frames[this.maxFrames + 1] !== 'X' && this.frameRoll > 1)
          return true;
        if (this.frames[this.maxFrames + 1] === 'X') return true;
      }
    }
    return false;
  }

  /**
   * Roll the ball and knock down pins.
   * @param {number} pins - The number of pins knocked down.
   * @throws {Error} If the number of pins is invalid or the game is over.
   */
  roll(pins) {
    if (pins < 0) {
      throw new Error('Negative roll is invalid');
    }

    if (pins > this.remainingPins) {
      throw new Error('Pin count exceeds pins on the lane');
    }

    if (this.isGameOver()) {
      throw new Error('Cannot roll after game is over');
    }

    this.incrementScore(pins);

    if (this.frameRoll === 1) {
      if (pins === this.maxPins) {
        this.scoreStrike();
        this.initializeFrame();
      } else {
        this.scoreFirstRoll(pins);
      }
    } else {
      if (pins === this.remainingPins) {
        this.scoreSpare(pins);
      } else {
        this.scoreOpenFrame(pins);
      }
      this.initializeFrame();
    }
  }

  /**
   * Calculate the total score of the game.
   * @returns {number} - The total score.
   * @throws {Error} If the game is not over yet.
   */
  score() {
    if (!this.isGameOver()) {
      throw new Error('Score cannot be taken until the end of the game');
    }
    return this.frameScores.reduce((total, num) => total + num, 0);
  }
}

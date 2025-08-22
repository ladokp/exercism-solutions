/**
 * Class representing an error for invalid input.
 * @extends Error
 */
export class InvalidInputError extends Error {
  /**
   * Create an InvalidInputError.
   * @param {string} [message='Invalid Input'] - The error message.
   */
  constructor(message) {
    super();
    this.message = message || 'Invalid Input';
  }
}

/**
 * Class representing a Robot.
 */
export class Robot {
  /**
   * Get the instructions for the robot.
   * @param {string} s - The string of instructions.
   * @returns {Array<string>} The array of instruction methods.
   * @throws {InvalidInputError} If the instruction character is invalid.
   */
  static instructions(s) {
    return [...s].map((character) => {
      switch (character) {
        case 'L':
          return 'turnLeft';
        case 'R':
          return 'turnRight';
        case 'A':
          return 'advance';
        default:
          throw new InvalidInputError(
            `${character} is not a valid instruction character.`,
          );
      }
    });
  }

  /**
   * Create a Robot.
   */
  constructor() {
    this.coordinates = [0, 0];
    this.bearing = 'north';
  }

  /**
   * Set the direction of the robot.
   * @param {string} next - The next direction.
   * @throws {InvalidInputError} If the direction is invalid.
   */
  set direction(next) {
    const validDirections = ['north', 'south', 'east', 'west'];
    if (!validDirections.includes(next)) {
      throw new InvalidInputError('Invalid Robot Bearing');
    }

    this.bearing = next;
  }

  /**
   * Advance the robot in the current direction.
   */
  advance() {
    switch (this.bearing) {
      case 'north':
        this.coordinates[1] += 1;
        break;
      case 'south':
        this.coordinates[1] -= 1;
        break;
      case 'east':
        this.coordinates[0] += 1;
        break;
      case 'west':
        this.coordinates[0] -= 1;
        break;
      default:
        console.warn(`Unknown bearing: ${this.bearing}`);
        break;
    }
  }

  /**
   * Turn the robot to the left.
   */
  turnLeft() {
    switch (this.bearing) {
      case 'north':
        this.direction = 'west';
        break;
      case 'south':
        this.direction = 'east';
        break;
      case 'east':
        this.direction = 'north';
        break;
      case 'west':
        this.direction = 'south';
        break;
      default:
        console.warn(`Unknown bearing: ${this.bearing}`);
        break;
    }
  }

  /**
   * Turn the robot to the right.
   */
  turnRight() {
    switch (this.bearing) {
      case 'north':
        this.direction = 'east';
        break;
      case 'south':
        this.direction = 'west';
        break;
      case 'east':
        this.direction = 'south';
        break;
      case 'west':
        this.direction = 'north';
        break;
      default:
        console.warn(`Unknown bearing: ${this.bearing}`);
        break;
    }
  }

  /**
   * Place the robot at specific coordinates and direction.
   * @param {Object} args - The placement arguments.
   * @param {number} args.x - The x-coordinate.
   * @param {number} args.y - The y-coordinate.
   * @param {string} args.direction - The direction.
   */
  place(args) {
    this.coordinates = [args.x, args.y];
    this.direction = args.direction;
  }

  /**
   * Evaluate a string of instructions.
   * @param {string} s - The string of instructions.
   */
  evaluate(s) {
    Robot.instructions(s).forEach((instruction) => {
      this[instruction]();
    });
  }
}

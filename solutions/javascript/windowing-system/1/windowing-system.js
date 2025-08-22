// @ts-check

/**
 * Class representing a Size with width and height.
 */
export class Size {
  /**
   * Creates an instance of Size.
   * @param {number} [width=80] - The width of the size.
   * @param {number} [height=60] - The height of the size.
   */
  constructor(width = 80, height = 60) {
    this.width = width;
    this.height = height;
  }

  /**
   * Resizes the Size instance.
   * @param {number} newWidth - The new width.
   * @param {number} newHeight - The new height.
   */
  resize(newWidth, newHeight) {
    if (newWidth < 1 || newHeight < 1) {
      throw new Error("Width and height must be positive numbers.");
    }
    this.width = newWidth;
    this.height = newHeight;
  }
}

/**
 * Class representing a Position with x and y coordinates.
 */
export class Position {
  /**
   * Creates an instance of Position.
   * @param {number} [x=0] - The x-coordinate.
   * @param {number} [y=0] - The y-coordinate.
   */
  constructor(x = 0, y = 0) {
    this.x = x;
    this.y = y;
  }

  /**
   * Moves the Position instance.
   * @param {number} newX - The new x-coordinate.
   * @param {number} newY - The new y-coordinate.
   */
  move(newX, newY) {
    if (newX < 0 || newY < 0) {
      throw new Error("Coordinates must be non-negative numbers.");
    }
    this.x = newX;
    this.y = newY;
  }
}

export class ProgramWindow {
  /**
   * Creates an instance of ProgramWindow with default values.
   */
  constructor() {
    this.screenSize = new Size(800, 600);
    this.size = new Size();
    this.position = new Position();
  }

  /**
   * Resizes the program window.
   * @param {Size} newSize - The new size.
   */
  resize(newSize) {
    if (!(newSize instanceof Size)) {
      throw new Error("Parameter must be an instance of Size.");
    }

    const targetWidth = Math.max(1, newSize.width);
    const maxWidth = this.screenSize.width - this.position.x;
    const newWidth = Math.min(targetWidth, maxWidth);

    const targetHeight = Math.max(1, newSize.height);
    const maxHeight = this.screenSize.height - this.position.y;
    const newHeight = Math.min(targetHeight, maxHeight);

    this.size.resize(newWidth, newHeight);
  }

  /**
   * Moves the program window.
   * @param {Position} newPosition - The new position.
   */
  move(newPosition) {
    if (!(newPosition instanceof Position)) {
      throw new Error("Parameter must be an instance of Position.");
    }

    const targetX = Math.max(0, newPosition.x);
    const maxX = this.screenSize.width - this.size.width;
    const newX = Math.min(targetX, maxX);

    const targetY = Math.max(0, newPosition.y);
    const maxY = this.screenSize.height - this.size.height;
    const newY = Math.min(targetY, maxY);

    this.position.move(newX, newY);
  }
}

/**
 * Adjusts the size and position of the given program window.
 * @param {ProgramWindow} programWindow - The program window to adjust.
 * @returns {ProgramWindow} The adjusted program window.
 */
export function changeWindow(programWindow) {
  if (!(programWindow instanceof ProgramWindow)) {
    throw new Error("Parameter must be an instance of ProgramWindow.");
  }

  const newSize = new Size(400, 300);
  programWindow.resize(newSize);

  const newPosition = new Position(100, 150);
  programWindow.move(newPosition);

  return programWindow;
}

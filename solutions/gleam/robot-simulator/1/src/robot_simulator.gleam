import gleam/list
import gleam/string

// Define the Robot type with direction and position
pub type Robot {
  Robot(direction: Direction, position: Position)
}

// Define the Direction type with four possible values
pub type Direction {
  North
  East
  South
  West
}

// Define the Position type with x and y coordinates
pub type Position {
  Position(x: Int, y: Int)
}

// Function to create a new Robot with a given direction and position
pub fn create(direction: Direction, position: Position) -> Robot {
  Robot(direction, position)
}

// Function to move the robot based on a sequence of instructions
pub fn move(
  direction: Direction,
  position: Position,
  instructions: String,
) -> Robot {
  let robot = create(direction, position)
  instructions
  |> string.to_graphemes // Split instructions into individual graphemes (characters)
  |> list.fold(from: robot, with: move_one) // Fold over the instructions to update the robot state
}

// Function to handle a single instruction and update the robot's state
fn move_one(robot: Robot, instruction: String) -> Robot {
  let Robot(direction, position) = robot
  case instruction {
    "L" -> Robot(turn_left(direction), position) // Turn left
    "R" -> Robot(turn_right(direction), position) // Turn right
    "A" -> Robot(direction, advance(direction, position)) // Advance in the current direction
    _ -> robot // Ignore invalid instructions
  }
}

// Function to turn the robot left
fn turn_left(direction: Direction) -> Direction {
  case direction {
    North -> West
    East -> North
    South -> East
    West -> South
  }
}

// Function to turn the robot right
fn turn_right(direction: Direction) -> Direction {
  case direction {
    North -> East
    East -> South
    South -> West
    West -> North
  }
}

// Function to advance the robot in the current direction
fn advance(direction: Direction, position: Position) -> Position {
  let Position(x, y) = position
  case direction {
    North -> Position(x, y + 1) // Move north
    East -> Position(x + 1, y) // Move east
    South -> Position(x, y - 1) // Move south
    West -> Position(x - 1, y) // Move west
  }
}

"""
This module defines a robot that can be moved around on a grid.
"""

EAST = 0
NORTH = 1
WEST = 2
SOUTH = 3


class Robot:
    """
    A class to represent a robot with position and direction on a grid.
    """

    def __init__(
        self,
        direction: int = NORTH,
        x_coordinate: int = 0,
        y_coordinate: int = 0,
    ) -> None:
        """
        Initializes the robot with a given direction and coordinates.

        :param direction: Initial direction the robot is facing (default is NORTH).
        :param x_coordinate: Initial x-axis coordinate of the robot (default is 0).
        :param y_coordinate: Initial y-axis coordinate of the robot (default is 0).
        """
        self.direction: int = direction
        self.x_coordinate: int = x_coordinate
        self.y_coordinate: int = y_coordinate
        self.coordinates: tuple[int, int] = (
            self.x_coordinate,
            self.y_coordinate,
        )

    def move(self, instructions: str = "") -> None:
        """
        Moves the robot on the grid based on the given instructions.

        :param instructions: A string of instructions where 'R' turns the robot right,
                             'L' turns the robot left, and 'A' moves the robot forward in
                             its current direction.
        :raises ValueError: If an invalid instruction is provided.
        """
        moves: dict[int, tuple[int, int]] = {
            EAST: (1, 0),
            NORTH: (0, 1),
            WEST: (-1, 0),
            SOUTH: (0, -1),
        }
        for instruction in instructions:
            if instruction == "R":
                self.direction = (self.direction - 1) % 4
            elif instruction == "L":
                self.direction = (self.direction + 1) % 4
            elif instruction == "A":
                delta_x, delta_y = moves[self.direction]
                self.x_coordinate += delta_x
                self.y_coordinate += delta_y
                self.coordinates = self.x_coordinate, self.y_coordinate
            else:
                raise ValueError("Wrong Instruction Given To Robot!")

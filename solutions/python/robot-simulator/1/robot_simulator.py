EAST = 0
NORTH = 1
WEST = 2
SOUTH = 3


class Robot:
    def __init__(self, direction=NORTH, x=0, y=0):
        self.direction = direction
        self.x = x
        self.y = y
        self.coordinates = self.x, self.y

    def move(self, instructions=""):
        for instruction in instructions:
            if instruction == "R":
                self.direction = (self.direction - 1) % 4
            elif instruction == "L":
                self.direction = (self.direction + 1) % 4
            elif instruction == "A":
                if self.direction == EAST:
                    self.x += 1
                if self.direction == NORTH:
                    self.y += 1
                if self.direction == WEST:
                    self.x += -1
                if self.direction == SOUTH:
                    self.y += -1
                self.coordinates = self.x, self.y
            else:
                raise ValueError("Wrong Instruction Given To Robot!")

"""Solution to Ellen's Alien Game exercise."""


class Alien:
    """Create an Alien object with location x_coordinate and y_coordinate.

    Attributes
    ----------
    total_aliens_created : int
        Class variable that counts the number of Alien instances created.
    x_coordinate : int
        Position on the x-axis.
    y_coordinate : int
        Position on the y-axis.
    health : int
        Amount of health points.

    Methods
    -------
    hit()
        Decrement Alien health by one point.
    is_alive() -> bool
        Return a boolean for if Alien is alive (if health is > 0).
    teleport(new_x_coordinate: int, new_y_coordinate: int)
        Move Alien object to new coordinates.
    collision_detection(other)
        Implementation TBD.
    """
    total_aliens_created = 0

    def __init__(self, initial_x, initial_y, health=3):
        """Initialize an Alien instance.

        Parameters
        ----------
        initial_x : int
            Initial position on the x-axis.
        initial_y : int
            Initial position on the y-axis.
        health : int, optional
            Initial health points (default is 3).
        """
        self.x_coordinate = initial_x
        self.y_coordinate = initial_y
        self.health = health
        Alien.total_aliens_created += 1

    def hit(self):
        """Decrement the Alien's health by one."""
        self.health -= 1

    def is_alive(self):
        """Check if the Alien is still alive.

        Returns
        -------
        bool
            True if health is greater than 0, False otherwise.
        """
        return self.health > 0

    def teleport(self, new_x_coordinate, new_y_coordinate):
        """Teleport the Alien to new coordinates.

        Parameters
        ----------
        new_x_coordinate : int
            New position on the x-axis.
        new_y_coordinate : int
            New position on the y-axis.
        """
        self.x_coordinate = new_x_coordinate
        self.y_coordinate = new_y_coordinate

    def collision_detection(self, other):
        """Detect collision with another object.

        Implementation TBD.

        Parameters
        ----------
        other : object
            Another object to detect collision with.
        """
        pass


def new_aliens_collection(positions):
    """Create a list of Alien objects from a list of positions.

    Parameters
    ----------
    positions : list of tuple
        List of (x, y) tuples representing positions.

    Returns
    -------
    list of Alien
        List of Alien objects initialized at the given positions.
    """
    return list(
        map(lambda position: Alien(position[0], position[1]), positions)
    )

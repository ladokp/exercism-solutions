"""
This module defines a Robot class that can generate and manage unique robot names.
"""

import secrets
import string


class Robot:
    """
    Robot class to generate and track unique names for robots.
    """

    used_robot_names = set()

    def __init__(self, max_attempts=10):
        """
        Initialize a Robot instance and assign a unique name.

        :param max_attempts: Maximum number of attempts to generate a unique name
        """
        self.max_attempts = max_attempts
        self.name = None
        self.reset()

    def generate_name(self):
        """
        Generate a unique robot name. Raises an error after exceeding max attempts.

        :return: A unique robot name as a string
        :raises RuntimeError: If a unique name cannot be generated
        """
        while (attempts := 0) < self.max_attempts:
            new_name = self.random_name()
            if new_name not in self.used_robot_names:
                return new_name
            attempts += 1
        raise RuntimeError(
            "Failed to generate a unique name after several attempts."
        )

    @staticmethod
    def random_name():
        """
        Generate a random robot name consisting of two uppercase letters followed by three digits.

        :return: A randomly generated robot name as a string
        """
        return "".join(
            secrets.choice(string.ascii_uppercase) for _ in range(2)
        ) + "".join(secrets.choice(string.digits) for _ in range(3))

    def reset(self):
        """
        Reset the robot's name to a newly generated unique name.
        """
        self.name = self.generate_name()
        self.used_robot_names.add(self.name)

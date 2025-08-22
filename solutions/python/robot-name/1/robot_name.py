import secrets
import string


class Robot(object):
    used_robot_names = set()

    def __init__(self):
        self.name = None
        self.reset()

    def generate_name(self):
        while True:
            if (new_name := self.random_name()) not in self.used_robot_names:
                return new_name

    @staticmethod
    def random_name():
        return (
            secrets.choice(string.ascii_uppercase)
            + secrets.choice(string.ascii_uppercase)
            + str(secrets.choice(range(100, 1000)))
        )

    def reset(self):
        self.name = self.generate_name()
        self.used_robot_names.add(self.name)

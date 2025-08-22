"""
Module for managing high scores in a game.
"""


class HighScores:
    """Class for representing and analyzing a list of high scores."""

    def __init__(self, scores):
        """
        Initialize the HighScores object with a list of scores.

        :param scores: A list of integer scores.
        """
        self.scores = scores

    def latest(self):
        """
        Get the latest score.

        :return: The most recent score.
        """
        return self.scores[-1]

    def personal_best(self):
        """
        Get the highest score.

        :return: The highest score from the list of scores.
        """
        return max(self.scores)

    def personal_top_three(self):
        """
        Get the top three scores in descending order.

        :return: A list of the top three scores.
        """
        return sorted(self.scores, reverse=True)[0:3]

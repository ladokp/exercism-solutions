"""
This module implements a Hangman game.
"""

# Game status categories
STATUS_WIN = "win"
STATUS_LOSE = "lose"
STATUS_ONGOING = "ongoing"


class Hangman:
    """
    A class to represent the game of Hangman.
    """

    def __init__(self, word):
        """
        Initialize the Hangman game with a word.

        :param word: The word to be guessed in the game.
        """
        self.remaining_guesses = 9
        self.status = STATUS_ONGOING
        self.guesses = []
        self.word = word

    def guess(self, character):
        """
        Make a guess in the game.

        :param character: The character being guessed.
        :raises ValueError: If the game has already ended.
        """
        if self.status != STATUS_ONGOING:
            raise ValueError("The game has already ended.")
        if character not in self.word or character in self.guesses:
            self.remaining_guesses -= 1
        else:
            self.guesses.append(character)
        self.update_status()

    def update_status(self):
        """
        Update the status of the game based on current guesses and remaining guesses.
        """
        if set(self.guesses) == set(self.word):
            self.status = STATUS_WIN
        elif self.remaining_guesses < 0:
            self.status = STATUS_LOSE

    def get_masked_word(self):
        """
        Get the current state of the word being guessed, with unguessed letters masked.

        :return: A string representing the masked word.
        """
        return "".join(
            char if char in self.guesses else "_" for char in self.word
        )

    def get_status(self):
        """
        Get the current status of the game.

        :return: The game's status, which can be 'win', 'lose', or 'ongoing'.
        """
        return self.status

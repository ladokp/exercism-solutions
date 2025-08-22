# Game status categories
STATUS_WIN = "win"
STATUS_LOSE = "lose"
STATUS_ONGOING = "ongoing"


class Hangman:
    def __init__(self, word):
        self.remaining_guesses = 9
        self.status = STATUS_ONGOING
        self.guesses = []
        self.word = word

    def guess(self, character):
        if self.status != STATUS_ONGOING:
            raise ValueError("The game has already ended.")
        if character not in self.word or character in self.guesses:
            self.remaining_guesses -= 1
        else:
            self.guesses.append(character)
        self.update_status()

    def update_status(self):
        if set(self.guesses) == set(self.word):
            self.status = STATUS_WIN
        elif self.remaining_guesses < 0:
            self.status = STATUS_LOSE

    def get_masked_word(self):
        return "".join(
            char if char in self.guesses else "_" for char in self.word
        )

    def get_status(self):
        return self.status

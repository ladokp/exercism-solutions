class Luhn:
    def __init__(self, card_number: str):
        self.card_number = str(card_number).replace(" ", "")

    def valid(self):
        if not self.card_number.isdigit() or len(self.card_number) <= 1:
            return False
        total = 0
        for index, digit in enumerate(self.card_number[::-1]):
            digit = int(digit)
            if index % 2 == 1:
                digit *= 2
                if digit > 9:
                    digit -= 9
            total += digit
        return total % 10 == 0

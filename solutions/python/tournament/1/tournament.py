from collections import defaultdict


class Team:
    def __init__(self):
        self.name = None
        self.matches_played = 0
        self.matches_won = 0
        self.matches_drawn = 0
        self.matches_lost = 0
        self.match_points = 0

    def add_draw(self):
        self.matches_played += 1
        self.matches_drawn += 1
        self.match_points += 1

    def add_win(self):
        self.matches_played += 1
        self.matches_won += 1
        self.match_points += 3

    def add_loss(self):
        self.matches_played += 1
        self.matches_lost += 1

    def __str__(self):
        return f"{self.name:<30} | {self.matches_played:>2} | {self.matches_won:>2} | {self.matches_drawn:>2} | {self.matches_lost:>2} | {self.match_points:>2}"


HEADER_LINE = "Team                           | MP |  W |  D |  L |  P"


def tally(rows):
    table_dict = defaultdict(lambda: Team())
    for row in rows:
        first_team, second_team, result = row.split(";")
        winner = table_dict[first_team]
        if winner.name is None:
            winner.name = first_team
        loser = table_dict[second_team]
        if loser.name is None:
            loser.name = second_team
        if result == "loss":
            winner = table_dict.get(second_team)
            loser = table_dict.get(first_team)
        if result == "draw":
            winner.add_draw()
            loser.add_draw()
        else:
            winner.add_win()
            loser.add_loss()
    table_lines = tuple(
        str(team)
        for team in sorted(
            table_dict.values(),
            key=lambda team: team.match_points,
            reverse=True,
        )
    )
    return [HEADER_LINE, *table_lines]

from collections import defaultdict


class Team:
    def __init__(self):
        self.name = None
        self.matches_won = 0
        self.matches_drawn = 0
        self.matches_lost = 0

    @property
    def matches_played(self):
        return self.matches_won + self.matches_drawn + self.matches_lost

    @property
    def match_points(self):
        return 3 * self.matches_won + self.matches_drawn

    def add_draw(self):
        self.matches_drawn += 1

    def add_win(self):
        self.matches_won += 1

    def add_loss(self):
        self.matches_lost += 1

    def __str__(self):
        return (
            f"{self.name:<30}"
            f" | {self.matches_played:>2}"
            f" | {self.matches_won:>2}"
            f" | {self.matches_drawn:>2}"
            f" | {self.matches_lost:>2}"
            f" | {self.match_points:>2}"
        )


def tally(rows):
    table_dict = defaultdict(Team)
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
    return [
        "Team                           | MP |  W |  D |  L |  P",
        *tuple(
            str(team)
            for team in sorted(
                table_dict.values(),
                key=lambda team: (-team.match_points, team.name),
            )
        ),
    ]

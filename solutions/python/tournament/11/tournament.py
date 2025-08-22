"""calculate a football table from game results"""
from collections import defaultdict
from enum import Enum


class Result(Enum):
    WIN = 1
    DRAW = 2
    LOSS = 3


class Team:
    """
    a class representing a football team
    """

    def __init__(self, name=None):
        self.name = name
        self._matches_won = self._matches_drawn = self._matches_lost = 0

    @property
    def matches_played(self):
        """return matches played count"""
        return self._matches_won + self._matches_drawn + self._matches_lost

    @property
    def match_points(self):
        """return the team's match points"""
        return 3 * self._matches_won + self._matches_drawn

    @property
    def sort_criteria(self):
        """return sort criteria for the table"""
        return -self.match_points, self.name

    def __add__(self, result):
        if not isinstance(result, Result):
            raise TypeError(f"Result has wrong type: {result}")
        match result:
            case Result.WIN:
                self._matches_won += 1
            case Result.DRAW:
                self._matches_drawn += 1
            case Result.LOSS:
                self._matches_lost += 1

    def __str__(self):
        return (
            f"{self.name:<30}"
            f" | {self.matches_played:>2}"
            f" | {self._matches_won:>2}"
            f" | {self._matches_drawn:>2}"
            f" | {self._matches_lost:>2}"
            f" | {self.match_points:>2}"
        )


def tally(rows):
    """calculate the given games result table"""
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
            winner += Result.DRAW
            loser += Result.DRAW
        else:
            winner += Result.WIN
            loser += Result.LOSS
    return [
        "Team                           | MP |  W |  D |  L |  P",
        *tuple(
            str(team)
            for team in sorted(
                table_dict.values(),
                key=lambda team: team.sort_criteria,
            )
        ),
    ]

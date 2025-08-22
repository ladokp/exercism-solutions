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
        first_team_name, second_team_name, result = row.split(";")
        first_teams_result, second_teams_result = Result.WIN, Result.LOSS
        first_team = table_dict[first_team_name]
        if first_team.name is None:
            first_team.name = first_team_name
        second_team = table_dict[second_team_name]
        if second_team.name is None:
            second_team.name = second_team_name
        if result == "loss":
            first_teams_result, second_teams_result = Result.LOSS, Result.WIN
        if result == "draw":
            first_teams_result = second_teams_result = Result.DRAW
        first_team += first_teams_result
        second_team += second_teams_result
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

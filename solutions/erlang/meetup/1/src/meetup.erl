-module(meetup).

-export([meetup/4]).

meetup(Year, Month, DayOfWeek, Week) ->
  Weekdays = [{monday, 1}, {tuesday, 2}, {wednesday, 3}, {thursday, 4}, {friday, 5}, {saturday, 6}, {sunday, 7}],
  LastDay = calendar:last_day_of_the_month(Year, Month),

  FirstDayOfMonth = calendar:day_of_the_week(Year, Month, 1),
  Difference = proplists:get_value(DayOfWeek, Weekdays) - FirstDayOfMonth,
  FirstOccurence = ((Difference + 7) rem 7) + 1,

  case Week of
    first -> {Year, Month, FirstOccurence};
    second -> {Year, Month, FirstOccurence + 7};
    third -> {Year, Month, FirstOccurence + 14};
    fourth -> {Year, Month, FirstOccurence + 21};
    teenth when (FirstOccurence + 7) =:= 13 -> {Year, Month, FirstOccurence + 7};
    teenth -> {Year, Month, FirstOccurence + 14};
    last when (FirstOccurence + 28) =< LastDay -> {Year, Month, FirstOccurence + 28};
    last -> {Year, Month, FirstOccurence + 21};
    _ -> error("Invalid arguments")
  end.

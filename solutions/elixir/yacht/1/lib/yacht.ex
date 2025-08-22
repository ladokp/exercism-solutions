defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(:yacht, [x, x, x, x, x]), do: 50
  def score(:ones, dice), do: count(dice, 1)
  def score(:twos, dice), do: count(dice, 2)
  def score(:threes, dice), do: count(dice, 3)
  def score(:fours, dice), do: count(dice, 4)
  def score(:fives, dice), do: count(dice, 5)
  def score(:sixes, dice), do: count(dice, 6)
  def score(:full_house, dice) do
    case Enum.sort(dice) do
      [x, x, y, y, y] when x != y -> Enum.sum(dice)
      [x, x, x, y, y] when x != y -> Enum.sum(dice)
      _ -> 0
    end
  end
  def score(:four_of_a_kind, dice) do
    case Enum.sort(dice) do
      [x, x, x, x, _y] -> x * 4
      [_x, y, y, y, y] -> y * 4
      _ -> 0
    end
  end
  def score(:little_straight, dice) do
    case Enum.sort(dice) do
      [1, 2, 3, 4, 5] -> 30
      _ -> 0
    end
  end
  def score(:big_straight, dice) do
    case Enum.sort(dice) do
      [2, 3, 4, 5, 6] -> 30
      _ -> 0
    end
  end
  def score(:choice, dice), do: Enum.sum(dice)
  def score(_, _), do: 0

  defp count(dice, num), do: Enum.filter(dice, &(&1 == num)) |> Enum.sum()
end
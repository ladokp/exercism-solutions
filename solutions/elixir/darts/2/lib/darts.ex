defmodule Darts do
  @type position :: {number, number}

  @doc """
  Calculate the score of a single dart hitting a target
  """
  @spec score(position) :: integer
  def score({x, y}) do
    distance_from_center = distance({x, y})
    cond do
      distance_from_center <= 1 -> 10
      distance_from_center <= 5 -> 5
      distance_from_center <= 10 -> 1
      true -> 0
    end
  end

  def distance({x, y}) do
    (:math.pow(Kernel.abs(x),2) + :math.pow(Kernel.abs(y), 2)) |> :math.sqrt
  end
end

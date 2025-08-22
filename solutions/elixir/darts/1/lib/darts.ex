defmodule Darts do
  @type position :: {number, number}

  @doc """
  Calculate the score of a single dart hitting a target
  """
  @spec score(position) :: integer
  def score({x, y}) do
    distanceFromCenter = distance({x, y})
    cond do
      distanceFromCenter <= 1 -> 10
      distanceFromCenter <= 5 -> 5
      distanceFromCenter <= 10 -> 1
      true -> 0
    end
  end

  def distance({x, y}) do
    (:math.pow(Kernel.abs(x),2) + :math.pow(Kernel.abs(y), 2)) |> :math.sqrt
  end
end

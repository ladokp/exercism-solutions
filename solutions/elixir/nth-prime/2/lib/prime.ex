defmodule Prime do
  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(0), do: raise "There's no 0th prime"
  def nth(count) do
    Stream.unfold(2, fn n-> {n, n+1} end)
    |> Stream.filter(&prime?/1)
    |> Stream.drop(count - 1)
    |> Stream.take(1)
    |> Enum.at(0)
  end

  defp prime?(1), do: false
  defp prime?(number) when number in  [2, 3], do: true
  defp prime?(number) do
    limit = trunc(:math.sqrt(number))
    has_divisor = (2..limit)
    |> Enum.map(fn divisor -> rem(number, divisor) == 0 end)
    |> Enum.any?
    not has_divisor
  end
end

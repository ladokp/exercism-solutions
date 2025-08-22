defmodule Triangle do
  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  def kind(a, b, c), do: kind(Enum.sort([a, b, c]))

  defp kind([a, _, _]) when a <= 0, do: {:error, "all side lengths must be positive"}
  defp kind([a, b, c]) when a + b <= c, do: {:error, "side lengths violate triangle inequality"}
  defp kind([a, a, a]), do: {:ok, :equilateral}
  defp kind([b, b, _]), do: {:ok, :isosceles}
  defp kind([_, c, c]), do: {:ok, :isosceles}
  defp kind([_, _, _]), do: {:ok, :scalene}
end

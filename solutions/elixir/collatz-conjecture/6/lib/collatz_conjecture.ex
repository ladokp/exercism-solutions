defmodule CollatzConjecture do
  defguardp is_even(input) when input > 0 and rem(input, 2) == 0
  defguardp is_odd(input) when input > 0 and rem(input, 2) == 1
  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """
  @spec calc(input :: pos_integer()) :: non_neg_integer()
  def calc(1), do: 0
  def calc(input) when is_even(input), do: 1 + calc(div(input, 2))
  def calc(input) when is_odd(input), do: 1 + calc(3*input + 1)
end

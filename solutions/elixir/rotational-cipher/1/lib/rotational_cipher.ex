defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

  Example:
  iex> RotationalCipher.rotate("Attack at dawn", 13)
  "Nggnpx ng qnja"
  """
  @upper_case ?A..?Z
  @lower_case ?a..?z
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
      text |> to_charlist |> Enum.map(&(rotate_char(&1, shift))) |> to_string
  end

  defp rotate_char(char, shift) do
    cond do
      char in @lower_case -> rem(char - ?a + shift, 26) + ?a
      char in @upper_case -> rem(char - ?A + shift, 26) + ?A
      true -> char
    end
  end
end

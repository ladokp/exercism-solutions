defmodule Scrabble do
  @doc """
  Calculate the scrabble score for the word.
  """

  @spec score(String.t()) :: non_neg_integer
  def score(word) do
    word
    |> to_charlist()
    |> Enum.map(&letter_score/1)
    |> Enum.sum()
  end

  defp letter_score(letter) when letter in 'AEIOULNRSTaeioulnrst', do: 1
  defp letter_score(letter) when letter in 'DGdg', do: 2
  defp letter_score(letter) when letter in 'BCMPbcmp', do: 3
  defp letter_score(letter) when letter in 'FHVWYfhvwy', do: 4
  defp letter_score(letter) when letter in 'Kk', do: 5
  defp letter_score(letter) when letter in 'JXjx', do: 8
  defp letter_score(letter) when letter in 'QZqz', do: 10
  defp letter_score(_), do: 0
end
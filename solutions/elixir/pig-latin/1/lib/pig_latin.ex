defmodule PigLatin do
  @vowels 'aeiou'
  @sound_vowels 'yx'
  @exceptions ['qu']
  
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ")
    |> Enum.map(&String.to_charlist/1)
    |> Enum.map(&to_pig/1)
    |> Enum.join(" ")
  end

  defp to_pig([first | [_ | _]] = phrase) when first in @vowels, do: phrase ++ 'ay' |> List.to_string()
  defp to_pig([first | [second | _]] = phrase) when first in @sound_vowels and second not in @vowels, do: phrase ++ 'ay' |> List.to_string()
  defp to_pig([first | [second | rest]]) when [first, second] in @exceptions, do: to_pig(rest ++ [first, second])
  defp to_pig([first | rest]), do: to_pig(rest ++ [first])
end

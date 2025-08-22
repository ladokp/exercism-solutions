defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    downcase_base = base |> String.downcase
    sorted_base = sort_string(downcase_base)
    candidates
    |> Enum.filter(fn candidate -> 
                      downcase_candiate = String.downcase(candidate)
                      downcase_base != downcase_candiate 
                      && sorted_base == sort_string(downcase_candiate)
                  end)
  end

  @spec sort_string(String.t()) :: [String.t()]
  defp sort_string(string) do
    string
    |> String.graphemes
    |> Enum.sort
  end
end

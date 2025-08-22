defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    sorted_base = sort_string(base)
    downcase_base = base |> String.downcase
    candidates
    |> Enum.filter(fn candidate -> 
                      downcase_base != String.downcase(candidate) 
                      && sorted_base == sort_string(candidate)
                  end)
  end

  @spec sort_string(String.t()) :: [String.t()]
  defp sort_string(string) do
    string
    |> String.downcase
    |> String.graphemes
    |> Enum.sort
  end
end

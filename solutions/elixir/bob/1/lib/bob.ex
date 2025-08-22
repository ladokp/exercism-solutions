defmodule Bob do
  def hey(input) do
    cond do
      question?(input) && yelling?(input) -> "Calm down, I know what I'm doing!"
      question?(input) -> "Sure."
      yelling?(input) -> "Whoa, chill out!"
      blank?(input)-> "Fine. Be that way!"
      true -> "Whatever."
    end
  end

  defp question?(input), do: String.ends_with?(String.trim(input), "?")
  defp with_alpha?(input), do: String.upcase(input) != String.downcase(input)
  defp yelling?(input), do: with_alpha?(input) and input == String.upcase(input)
  defp blank?(input), do: String.trim(input) == ""
end
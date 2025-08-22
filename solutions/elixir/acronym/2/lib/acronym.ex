defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(text) do
      text
      |> String.split([" ", "-", "_"], trim: true)
      |> Enum.map(&String.first/1)
      |> Enum.map(&String.upcase/1)
      |> Enum.join
  end
end

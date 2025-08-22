defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
      string
      |> String.upcase
      |> String.replace("-", " ")
      |> String.replace("_", "")
      |> String.split(" ")
      |> Enum.map(fn x -> String.at(x, 0) end)
      |> Enum.join
  end
end

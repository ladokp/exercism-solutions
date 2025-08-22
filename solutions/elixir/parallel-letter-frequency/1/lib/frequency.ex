defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    texts
    |> Enum.join
    |> String.downcase
    |> String.graphemes
    |> Enum.filter(&String.match?(&1, ~r/[[:alpha:]]/))
    |> chunk(workers)
    |> Task.async_stream(&Enum.frequencies/1)
    |> Enum.reduce(
      %{},
      fn {:ok, current}, accumulator -> Map.merge(accumulator, current, fn _, value1, value2 -> value1 + value2 end) end
    )
  end

  @spec chunk(String.t(), pos_integer) :: map
  defp chunk(text, workers), do: Enum.chunk_every(text, max(div(Enum.count(text), workers), 1))
end
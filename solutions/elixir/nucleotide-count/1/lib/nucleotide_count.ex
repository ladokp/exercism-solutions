defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a DNA strand.

  ## Examples

  iex> NucleotideCount.count(~c"AATAA", ?A)
  4

  iex> NucleotideCount.count(~c"AATAA", ?T)
  1
  """
  @spec count(charlist(), char()) :: non_neg_integer()
  def count(strand, nucleotide) do
      histogram(strand)[nucleotide]
  end

  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram(~c"AATAA")
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram(charlist()) :: map()
  def histogram(strand) do
      strand
      |> Enum.reduce(
            %{71 => 0, 65 => 0, 67 => 0, 84 => 0},
            fn(letter, accumulator) ->
              count = Map.get(accumulator, letter) || 0
              Map.put(accumulator, letter, count+1)
            end
         )
  end
end

defmodule RnaTranscription do
  @dictionary %{ ?A => ?U,
                 ?C => ?G,
                 ?T => ?A,
                 ?G => ?C }

  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

    iex> RnaTranscription.to_rna(~c"ACTG")
    ~c"UGAC"
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    dna
    |> Enum.map(fn nucleotide -> @dictionary[nucleotide] end)
  end
end

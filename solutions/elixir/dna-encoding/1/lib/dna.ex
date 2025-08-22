defmodule DNA do
  def encode_nucleotide(?\s), do: 0b0000
  def encode_nucleotide(?A), do: 0b0001
  def encode_nucleotide(?C), do: 0b0010
  def encode_nucleotide(?G), do: 0b0100
  def encode_nucleotide(?T), do: 0b1000

  def decode_nucleotide(0b0000), do: ?\s
  def decode_nucleotide(0b0001), do: ?A
  def decode_nucleotide(0b0010), do: ?C
  def decode_nucleotide(0b0100), do: ?G
  def decode_nucleotide(0b1000), do: ?T

  def encode(dna), do: do_encode(dna, <<0::size(0)>>)

  defp do_encode([], accumulator), do: accumulator
  defp do_encode([current | remaining], accumulator), do: do_encode(remaining, <<accumulator::bitstring, encode_nucleotide(current)::4>>)

  def decode(dna), do: do_decode(dna, '')

  defp do_decode(<<0::0>>, accumulator), do: accumulator
  defp do_decode(<<current::4, remaining::bitstring>>, accumulator), do: do_decode(remaining, accumulator ++ [decode_nucleotide(current)])
end

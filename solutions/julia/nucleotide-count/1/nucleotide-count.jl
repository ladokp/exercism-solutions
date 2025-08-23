"""
    count_nucleotides(strand)

The count of each nucleotide within `strand` as a dictionary.

Invalid strands raise a `DomainError`.

"""
function count_nucleotides(strand)
    result = Dict('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0)
    [haskey(result, key) ? result[key] += 1 : throw(DomainError(key)) for key in strand]
    return result
end

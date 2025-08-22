def to_rna(dna_strand):
    return "".join(
        {"G": "C", "C": "G", "T": "A", "A": "U"}.get(character)
        for character in dna_strand
    )

LOOKUP = {"G": "C", "C": "G", "T": "A", "A": "U"}


def to_rna(dna_strand):
    return "".join(LOOKUP.get(character) for character in dna_strand)

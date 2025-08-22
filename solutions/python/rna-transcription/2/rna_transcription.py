TRANSLATION_MAP =  {"G": "C", "C": "G", "T": "A", "A": "U"}

def to_rna(dna_strand):
    return "".join(
        TRANSLATION_MAP.get(character)
        for character in dna_strand
    )

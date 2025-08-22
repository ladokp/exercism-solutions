"""
This module contains a function to translate a DNA strand into an RNA strand.
"""

LOOKUP = {"G": "C", "C": "G", "T": "A", "A": "U"}


def to_rna(dna_strand):
    """
    Converts a DNA strand into an RNA strand by replacing each nucleotide with its complement:
    G -> C, C -> G, T -> A, A -> U.

    Parameters:
    dna_strand (str): The DNA strand to be converted.

    Returns:
    str: The resulting RNA strand.
    """
    return "".join(LOOKUP.get(character) for character in dna_strand)

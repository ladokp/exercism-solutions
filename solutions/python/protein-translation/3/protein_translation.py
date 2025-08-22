"""
RNA to Protein Translation Module

This module provides functionality to translate RNA sequences into proteins
by matching codons to their respective protein symbols or stop signals.
"""

from collections.abc import Generator


def generate_proteins(strand: str) -> Generator[str, None, None]:
    """
    Generates proteins from an RNA strand.

    This function parses the RNA strand in segments of three nucleotides (codons)
    and yields the corresponding protein name. It stops processing when a stop
    codon is encountered.

    Args:
        strand (str): The RNA strand composed of nucleotide sequences.

    Yields:
        str: The name of the protein corresponding to the current codon.
    """
    for index in range(0, len(strand), 3):
        protein = None
        match strand[slice(index, index + 3)]:
            case "AUG":
                protein = "Methionine"
            case "UUU" | "UUC":
                protein = "Phenylalanine"
            case "UUA" | "UUG":
                protein = "Leucine"
            case "UCU" | "UCC" | "UCA" | "UCG":
                protein = "Serine"
            case "UAU" | "UAC":
                protein = "Tyrosine"
            case "UGU" | "UGC":
                protein = "Cysteine"
            case "UGG":
                protein = "Tryptophan"
            case "UAA" | "UAG" | "UGA":
                return
        yield protein


def proteins(strand: str) -> list[str]:
    """
    Translates RNA sequences into a list of protein names.

    This function converts an entire RNA strand into a list of protein names
    by invoking the 'generate_proteins' generator function.

    Args:
        strand (str): The RNA strand composed of nucleotide sequences.

    Returns:
        list[str]: A list containing the names of the proteins translated from the RNA strand.
    """
    return list(generate_proteins(strand))

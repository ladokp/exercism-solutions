translation_table = {
    "AUG": "Methionine",
    "UUU": "Phenylalanine",
    "UUC": "Phenylalanine",
    "UUA": "Leucine",
    "UUG": "Leucine",
    "UCU": "Serine",
    "UCC": "Serine",
    "UCA": "Serine",
    "UCG": "Serine",
    "UAU": "Tyrosine",
    "UAC": "Tyrosine",
    "UGU": "Cysteine",
    "UGC": "Cysteine",
    "UGG": "Tryptophan",
    "UAA": None,
    "UAG": None,
    "UGA": None,
}


def proteins(strand):
    proteins_list = [
        translation_table[strand[index : index + 3]]
        for index in range(0, len(strand), 3)
    ]
    if None in proteins_list:
        return proteins_list[0 : proteins_list.index(None)]
    return proteins_list

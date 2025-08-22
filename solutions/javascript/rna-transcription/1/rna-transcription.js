export function toRna(dna) {
    var rna = "";
    [...dna].forEach((nucleotide) => {
        switch (nucleotide) {
            case 'G':
                rna += 'C';
                break;
            case 'C':
                rna += 'G';
                break;
            case 'T':
                rna += 'A';
                break;
            case 'A':
                rna += 'U';
                break;
        }
    })
    return rna;
};
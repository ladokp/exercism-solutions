package strand

var rnaTranslationMap = map[rune]byte{
	'G': 'C',
	'C': 'G',
	'T': 'A',
	'A': 'U',
}

func ToRNA(dna string) string {
	rna := make([]byte, len(dna))
	for index, character := range dna {
		rna[index] = rnaTranslationMap[character]
	}
	return string(rna)
}

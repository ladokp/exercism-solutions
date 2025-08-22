package strand

func ToRNA(dna string) string {
	rna := make([]byte, len(dna))
	translationMap := map[rune]byte{
		'G': 'C',
		'C': 'G',
		'T': 'A',
		'A': 'U',
	}
	for index, character := range dna {
		rna[index] = translationMap[character]
	}
	return string(rna)
}

package strand

func ToRNA(dna string) string {
	rna := make([]byte, len(dna))
	for index, character := range dna {
		rna[index] = map[rune]byte{
			'G': 'C',
			'C': 'G',
			'T': 'A',
			'A': 'U',
		}[character]
	}
	return string(rna)
}

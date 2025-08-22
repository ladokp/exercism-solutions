package protein

import "errors"

const (
	Stop          = "STOP"
	Methionine    = "Methionine"
	Phenylalanine = "Phenylalanine"
	Leucine       = "Leucine"
	Serine        = "Serine"
	Tyrosine      = "Tyrosine"
	Cysteine      = "Cysteine"
	Tryptophan    = "Tryptophan"
)

var (
	ErrStop        error = errors.New("error stop")
	ErrInvalidBase error = errors.New("error invalid base")
	// CodonToProtein dictionary
	CodonToProtein = map[string]string{
		"AUG": Methionine,
		"UUU": Phenylalanine,
		"UUC": Phenylalanine,
		"UUA": Leucine,
		"UUG": Leucine,
		"UCU": Serine,
		"UCC": Serine,
		"UCA": Serine,
		"UCG": Serine,
		"UAU": Tyrosine,
		"UAC": Tyrosine,
		"UGU": Cysteine,
		"UGC": Cysteine,
		"UGG": Tryptophan,
		"UAA": Stop,
		"UAG": Stop,
		"UGA": Stop,
	}
)

// FromCodon transform codon to Protein value
func FromCodon(s string) (string, error) {
	value, found := CodonToProtein[s]
	if !found {
		return "", ErrInvalidBase
	}
	if value == Stop {
		return "", ErrStop
	}
	return value, nil
}

func FromRNA(s string) ([]string, error) {
	var result []string
	codons := splitByN(s, 3)
	for _, codon := range codons {
		protein, err := FromCodon(codon)
		switch err {
		case ErrStop:
			return result, nil
		case ErrInvalidBase:
			return result, err
		default:
			result = append(result, protein)
		}
	}
	return result, nil
}
func splitByN(s string, n int) []string {
	var codons []string
	length := len(s)
	for i := 0; i < length; i++ {
		if i%3 == 0 {
			if i+3 < length {
				codons = append(codons, s[i:i+3])
			} else {
				codons = append(codons, s[i:])
			}
		}
	}
	return codons
}

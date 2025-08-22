/**
 * A map of codons to their corresponding proteins.
 * @constant {Object<string, string>}
 */
const ACID_PROTEIN_MAP = {
  AUG: 'Methionine',
  UUU: 'Phenylalanine',
  UUC: 'Phenylalanine',
  UUA: 'Leucine',
  UUG: 'Leucine',
  UCU: 'Serine',
  UCC: 'Serine',
  UCA: 'Serine',
  UCG: 'Serine',
  UAU: 'Tyrosine',
  UAC: 'Tyrosine',
  UGU: 'Cysteine',
  UGC: 'Cysteine',
  UGG: 'Tryptophan',
  UAA: 'STOP',
  UAG: 'STOP',
  UGA: 'STOP',
};

/**
 * Gets the protein corresponding to a given codon.
 * @param {string} codon - The RNA codon.
 * @returns {string} The corresponding protein or 'INVALID' if the codon is not recognized.
 */
const getProtein = (codon) => ACID_PROTEIN_MAP[codon] || 'INVALID';

/**
 * Translates an RNA strand into a list of proteins.
 * @param {string} rnaStrand - The RNA strand to translate.
 * @returns {string[]} An array of proteins translated from the RNA strand.
 * @throws {Error} If an invalid codon is encountered in the RNA strand.
 */
export const translate = (rnaStrand) => {
  const proteins = [];

  if (rnaStrand) {
    for (let i = 0; i < rnaStrand.length; i += 3) {
      const protein = getProtein(rnaStrand.substring(i, i + 3));

      if (protein) {
        if (protein === 'STOP') {
          break;
        }

        if (protein === 'INVALID') {
          throw new Error('Invalid codon');
        }

        proteins.push(protein);
      }
    }
  }

  return proteins;
};

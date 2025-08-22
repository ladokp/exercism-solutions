function countNucleotides(strand) {
  const nucleotideCounts = { A: 0, C: 0, G: 0, T: 0 };

  for (const nucleotide of strand) {
    if (!(nucleotide in nucleotideCounts)) {
      throw new Error('Invalid nucleotide in strand');
    }
    nucleotideCounts[nucleotide]++;
  }

  return `${nucleotideCounts.A} ${nucleotideCounts.C} ${nucleotideCounts.G} ${nucleotideCounts.T}`;
}

export { countNucleotides };

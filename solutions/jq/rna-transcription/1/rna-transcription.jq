def toRna:
  reduce split("")[] as $dna (""; . + {G:"C", C:"G", T:"A", A:"U"}[$dna]);

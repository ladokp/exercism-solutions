module ProteinTranslation

open System

let codonToProtein = 
    function
    | "AUG" -> "Methionine"
    | "UGG" -> "Tryptophan"
    | "UGU" | "UGC" -> "Cysteine"
    | "UUA" | "UUG" -> "Leucine"
    | "UUC" | "UUU" -> "Phenylalanine"
    | "UAU" | "UAC" -> "Tyrosine"
    | "UAA" | "UAG" | "UGA" -> "STOP"
    | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
    | _ -> failwith "Invalid codon"

let proteins (rna: string) =
    rna 
    |> Seq.chunkBySize 3 
    |> Seq.map (String >> codonToProtein)
    |> Seq.takeWhile (fun str -> str <> "STOP")
    |> Seq.toList

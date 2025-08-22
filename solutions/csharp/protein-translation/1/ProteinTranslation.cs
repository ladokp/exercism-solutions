using System;
using System.Collections.Generic;

public static class ProteinTranslation
{
    private static string ConvertToProtein(string input)
    {
        switch (input)
        {
            case "AUG":
                return "Methionine";
            case "UGG":
                return "Tryptophan";
            case "UGU":
            case "UGC":
                return "Cysteine";
            case "UUA":
            case "UUG":
                return "Leucine";
            case "UUU":
            case "UUC":
                return "Phenylalanine";
            case "UAU":
            case "UAC":
                return "Tyrosine";
            case "UAA":
            case "UAG":
            case "UGA":
                return "STOP";
            case "UCU":
            case "UCC":
            case "UCA":
            case "UCG":
                return "Serine";
            default:
                throw new Exception("Invalid sequence");
        }
    }

    public static string[] Proteins(string strand)
    {
        var results = new List<string>();
        for (int index = 0; index < strand.Length / 3; index++)
        {
            var protein = ConvertToProtein(strand.Substring(3 * index, 3));
            if (protein == "STOP") break;
            results.Add(protein);
        }
        return results.ToArray();
    }
}
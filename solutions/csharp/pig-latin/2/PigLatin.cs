using System.Linq;
using System.Text.RegularExpressions;

public class PigLatin
{
    private static string TranslateWord(string word)
    {
        if (WordStartsWithVowelLike(word))
            return $"{word}ay";
        if (WordStartsWithPrefixes(word, "thr", "sch"))
            return word.Substring(3) + word.Substring(0, 3) + "ay";
        if (WordStartsWithPrefixes(word, "ch", "qu", "th", "rh"))
            return word.Substring(2) + word.Substring(0, 2) + "ay";
        if (WordStartsWithConsonantAndQu(word))
            return word.Substring(3) + word[0] + "quay";
        return word.Substring(1) + word[0] + "ay";
    }
    public static string Translate(string word) => string.Join(" ", word.Split(' ').Select(x => (TranslateWord(x))));
    private static bool WordStartsWithVowelLike(string word) => Regex.IsMatch("[aeiou]", word[0].ToString()) || word.StartsWith("yt") || word.StartsWith("xr");
    private static bool WordStartsWithPrefixes(string word, params string[] prefixes) => prefixes.Any(word.StartsWith);
    private static bool WordStartsWithConsonantAndQu(string word) => word.Substring(1).StartsWith("qu");
}

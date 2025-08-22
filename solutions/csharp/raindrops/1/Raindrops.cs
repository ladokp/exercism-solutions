using System.Text;

public static class Raindrops
{
    public static string Convert(int number)
    {
        var builder = new StringBuilder();
        if (number % 3 == 0) builder.Append("Pling");
        if (number % 5 == 0) builder.Append("Plang");
        if (number % 7 == 0) builder.Append("Plong");
        if (builder.Length == 0) return $"{number}";
        return builder.ToString();        
    }
}
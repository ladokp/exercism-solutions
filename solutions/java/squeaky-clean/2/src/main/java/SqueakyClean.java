import java.util.Locale;
import java.util.regex.Pattern;

class SqueakyClean {

  static String clean(String identifier) {

    var result = identifier.replaceAll(" ", "_")
        .replaceAll("0", "o")
        .replaceAll("1", "l")
        .replaceAll("3", "e")
        .replaceAll("4", "a")
        .replaceAll("7", "t")
        .replaceAll("\\p{C}", "CTRL")
        .replaceAll("[^\\p{L}\\p{P}]", "")
        .replaceAll("[α-ω]", "")
        .replaceAll("[^a-zA-Z_-]", "");

    var matcher = Pattern.compile("(.*)(-\\p{L})(.*)").matcher(result);
    if (matcher.matches()) {
      result = matcher.replaceAll(
          matcher.group(1)
              + matcher.group(2).toUpperCase(Locale.ROOT).substring(1)
              + matcher.group(3)
      );
    }

    return result;
  }
}
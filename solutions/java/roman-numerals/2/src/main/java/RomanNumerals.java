import java.util.*;

public class RomanNumerals {
    private int number;
    
    private static NavigableMap<Integer, String> values = new TreeMap<>();
    
    static {
        values.put(1_000,   "M");
        values.put(  900,  "CM");
        values.put(  500,   "D");
        values.put(  400,  "CD");
        values.put(  100,   "C");
        values.put(   90,  "XC");
        values.put(   50,   "L");
        values.put(   40,  "XL");
        values.put(   10,   "X");
        values.put(    9,  "IX");
        values.put(    5,   "V");
        values.put(    4,  "IV");
        values.put(    1,   "I");
        values = values.descendingMap();
    }

    public RomanNumerals(int number) {
        this.number = number;
    }
    
    String getRomanNumeral() {
        StringBuilder numeralBuilder = new StringBuilder();

        for (var entry : values.entrySet()) {
            String numeral = entry.getValue();
            int currentNumber = entry.getKey();

            while (this.number >= currentNumber) {
                this.number -= currentNumber;
                numeralBuilder.append(numeral);
            }
        }
        return numeralBuilder.toString();
    }
}
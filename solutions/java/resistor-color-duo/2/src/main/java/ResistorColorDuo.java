import java.util.List;
import java.util.Arrays;

class ResistorColorDuo {
    private final List<String> COLORS_LIST = Arrays.asList("black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white");
    
    int value(String[] colors) {
        if(colors.length < 2) {
            throw new UnsupportedOperationException("At least two colors have to be given.");
        }
        int colorOneValue = COLORS_LIST.indexOf(colors[0]);
        int colorTwoValue = COLORS_LIST.indexOf(colors[1]);
        
        return colorOneValue * 10 + colorTwoValue;
    }
}


import java.util.List;
import java.util.Arrays;

class ResistorColor {
    private final String[] COLORS_ARRAY = {"black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white"};
    private final List<String> COLORS_LIST = Arrays.asList(COLORS_ARRAY);
    
    int colorCode(String color) {
        return COLORS_LIST.indexOf(color);
    }

    String[] colors() {
        return this.COLORS_ARRAY;
    }
}

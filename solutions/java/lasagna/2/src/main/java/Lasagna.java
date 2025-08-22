public class Lasagna {
    private static int MINUTES_IN_OVEN = 40;
    private static int MINUTES_PER_LAYER = 2;
    
    public int expectedMinutesInOven() {
        return MINUTES_IN_OVEN;
    }
    
    public int remainingMinutesInOven(int actualMinutesInOven) {
        return expectedMinutesInOven() - actualMinutesInOven;
    }

    public int preparationTimeInMinutes(int numberOfLayers) {
        return MINUTES_PER_LAYER * numberOfLayers;
    }

    public int totalTimeInMinutes(int numberOfLayers, int actualMinutesInOven) {
        return preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven;
    }
}

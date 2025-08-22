public class Lasagna {
    int OVENT_TIME = 40;
    int TIME_PER_LAYER = 2;
    
    public int expectedMinutesInOven() {
        return OVENT_TIME;
    }
    
    public int remainingMinutesInOven(int actualTimeInOven) {
        return expectedMinutesInOven() - actualTimeInOven;
    }

    public int preparationTimeInMinutes(int numberOfLayers) {
        return TIME_PER_LAYER * numberOfLayers;
    }

    public int totalTimeInMinutes(int numberOfLayers, int actualTimeInOven) {
        return preparationTimeInMinutes(numberOfLayers) + actualTimeInOven;
    }
}

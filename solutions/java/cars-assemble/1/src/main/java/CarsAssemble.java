public class CarsAssemble {
    private static int HOURLY_PRODUCTION_RATE = 221;
    
    public double productionRatePerHour(int speed) {
        double successRate = 0;
        
        if (speed > 0 && speed <= 4) {
            successRate = 1.0;
        }
        else if (speed <= 8) {
            successRate = 0.9;
        }
        else if (speed == 9) {
            successRate = 0.8;
        }
        else if (speed == 10) {
            successRate = 0.77;
        }
        return HOURLY_PRODUCTION_RATE * speed * successRate;
    }

    public int workingItemsPerMinute(int speed) {
        return (int)(productionRatePerHour(speed) / 60);
    }
}

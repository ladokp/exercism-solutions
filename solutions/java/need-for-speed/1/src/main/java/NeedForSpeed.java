class NeedForSpeed {
    private int speed;
    private int batteryDrain;
    private int distance;
    private int batteryLevel = 100;
    
    NeedForSpeed(int speed, int batteryDrain) {
        this.speed = speed;
        this.batteryDrain = batteryDrain;
    }

    public boolean batteryDrained() {
        return batteryLevel < batteryDrain;
    }

    public int distanceDriven() {
        return distance;
    }

    public float maximalDistance() {
        return 100 * speed / batteryDrain ;
    }

    public void drive() {
        if (!batteryDrained()) {
            distance += speed;       
            batteryLevel -= batteryDrain;
        }
    }

    public static NeedForSpeed nitro() {
        return new NeedForSpeed(50, 4);
    }
}

class RaceTrack {
    private int distance;

    RaceTrack(int distance) {
        this.distance = distance;
    }

    public boolean tryFinishTrack(NeedForSpeed car) {
        return car.maximalDistance() >= distance;
    }
}

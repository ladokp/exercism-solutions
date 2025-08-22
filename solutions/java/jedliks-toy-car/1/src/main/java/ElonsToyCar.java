public class ElonsToyCar {
    private int distance;
    private int batteryLevel = 100;
    
    public static ElonsToyCar buy() {
        return new ElonsToyCar();
    }

    public String distanceDisplay() {
        return String.format("Driven %s meters", distance);
    }

    public String batteryDisplay() {
        return batteryLevel > 0 ? String.format("Battery at %s%%", batteryLevel) : "Battery empty";
    }

    public void drive() {
        if (100 >= batteryLevel && batteryLevel > 0) {
            distance += 20;
            batteryLevel -= 1;
        }
    }
}

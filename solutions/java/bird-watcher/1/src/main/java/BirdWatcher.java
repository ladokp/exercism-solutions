
class BirdWatcher {
    private final int[] birdsPerDay;

    public BirdWatcher(int[] birdsPerDay) {
        this.birdsPerDay = birdsPerDay.clone();
    }

    public int[] getLastWeek() {
        return new int[] { 0, 2, 5, 3, 7, 8, 4 };
    }

    public int getToday() {
        return this.birdsPerDay[6];
    }

    public void incrementTodaysCount() {
        this.birdsPerDay[6]++;
    }

    public boolean hasDayWithoutBirds() {
        for (int i = 0; i < this.birdsPerDay.length; i++) {
            if (this.birdsPerDay[i] == 0) return true;
        }
        return false;
    }

    public int getCountForFirstDays(int numberOfDays) {
        int sum = 0;
        int length = this.birdsPerDay.length;
        int days = numberOfDays < length ? numberOfDays : length;
        for (int i = 0; i < days; i++) {
            sum += this.birdsPerDay[i];
        }
        return sum;
    }

    public int getBusyDays() {
        int count = 0;
        for (int i = 0; i < this.birdsPerDay.length; i++) {
            if (this.birdsPerDay[i] >= 5) count++;
        }
        return count;
    }
}

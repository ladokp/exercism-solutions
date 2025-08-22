@FunctionalInterface
interface Predicate { 
    boolean test(int year); 
}

class Leap {

    Predicate checkForLeapYear = year -> year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);

    public boolean isLeapYear(int year) {
        return checkForLeapYear.test(year);
    }
    
}

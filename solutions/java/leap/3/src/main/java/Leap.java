@FunctionalInterface
interface LeapYearChecker { 
    boolean isLeapYear(int year); 
}

class Leap {

    LeapYearChecker checker;

    public Leap() {
        checker = (year) -> year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) ;
    }

    public boolean isLeapYear(int year) {
        return checker.isLeapYear(year);
    }
    
}

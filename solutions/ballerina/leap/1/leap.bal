// Function to check if a given year is a leap year
//
// + year - the year to check
// + return - true if the year is a leap year, false otherwise
public function isLeapYear(int year) returns boolean {
    if (year % 4 == 0) {
        if (year % 100 == 0) {
            if (year % 400 == 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    } else {
        return false;
    }
}

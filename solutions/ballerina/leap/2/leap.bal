// Function to check if a given year is a leap year
//
// + year - the year to check
// + return - true if the year is a leap year, false otherwise
public function isLeapYear(int year) returns boolean {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
}

// Search a sorted array for a value and return the index.
//
// + array - a sorted array of integers
// + value - the integer item to find
// + return - the index of the value, or nil if the value is not found
public function find(int[] array, int value) returns int? {
    int start = 0;
    int end = array.length() - 1;

    while start <= end {
        // Avoid overflow by calculating mid this way
        int mid = start + ((end - start) / 2);
        if value == array[mid] {
            return mid;
        }
        if value < array[mid] {
            end = mid - 1;
        } else {
            start = mid + 1;
        }
    }

    return;
}

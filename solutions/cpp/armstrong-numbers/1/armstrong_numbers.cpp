#include "armstrong_numbers.h"

namespace armstrong_numbers {

    bool is_armstrong_number(int candidate) {
        int length = log10(candidate) + 1;
        int sum = 0;

        for (int index = candidate; index > 0; index /= 10) {
            sum += pow(index % 10, length);
        }

        return candidate == sum;
    }

}  // namespace armstrong_numbers

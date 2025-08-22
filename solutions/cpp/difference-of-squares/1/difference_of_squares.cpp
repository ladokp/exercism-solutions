#include "difference_of_squares.h"

namespace difference_of_squares {
    double square_of_sum(int number) {
        return pow((number * (number + 1) / 2), 2);
    }

    double sum_of_squares(int number) {
        return number * (number + 1) * (2 * number + 1) / 6;
    }

    double difference(int number) {
        return square_of_sum(number) - sum_of_squares(number);
    }
}  // namespace difference_of_squares

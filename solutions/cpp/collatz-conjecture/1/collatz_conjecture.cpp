#include "collatz_conjecture.h"

#include <stdexcept>

namespace collatz_conjecture {
    int steps(int number) {
        if (number < 1) {
            throw std::domain_error("Only positive integers are allowed");
        }
        if (number == 1) {
            return 0;
        }
        if(number % 2 == 0) {
            return 1 + steps(number/2);
        }
        return 1 + steps(3 * number + 1); 
    }
}  // namespace collatz_conjecture

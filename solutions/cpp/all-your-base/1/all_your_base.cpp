#include "all_your_base.h"
#include <vector>
#include <stdexcept>

namespace all_your_base {

std::vector<unsigned int> convert(unsigned int input_base,
                                  const std::vector<unsigned int>& input_digits,
                                  unsigned int output_base) {
    unsigned int value = 0;
    std::vector<unsigned int> b;
    if (input_base <= 1 || output_base <= 1)
        throw std::invalid_argument("Invalid base");
    if (input_digits.empty())
        return {};
    for (unsigned int digit : input_digits) {
        if (digit >= input_base)
            throw std::invalid_argument("Invalid number");
        value = value * input_base + digit;
    }
    while (value != 0) {
        b.insert(b.begin(), value % output_base);
        value /= output_base;
    }
    return b;
}

}

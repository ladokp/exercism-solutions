#include "darts.h"

#include <cmath>

namespace darts {
    int score(double x, double y) {
        double distance = sqrt(pow(x, 2) + pow(y, 2));
        if (distance <= 1) {
            return 10;
        }
        if (distance <= 5) {
            return 5;
        }
        if (distance <= 10) {
            return 1;
        }
        return 0;
    }
} // namespace darts
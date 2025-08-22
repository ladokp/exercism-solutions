#include "triangle.h"
#include <stdexcept>

namespace triangle {
   bool all_sides_are_positive(double a, double b, double c)
    {
        return (a > 0) && (b > 0) && (c > 0);
    }

    bool satisfies_triangle_inequality(double a, double b, double c)
    {
        return (a + b >= c) && (b + c >= a) && (c + a >= b);
    }

    void assert_triangle_is_valid(double a, double b, double c)
    {
        if (!all_sides_are_positive(a, b, c) ||
            !satisfies_triangle_inequality(a, b, c))
            throw std::domain_error("invalid triangle");
    }

    bool is_equilateral(double a, double b, double c)
    {
        return (a == b) && (b == c);
    }

    bool is_isosceles(double a, double b, double c)
    {
        return (a == b) || (b == c) || (c == a);
    }
} // namespace

triangle::flavor triangle::kind(double a, double b, double c)
{
    assert_triangle_is_valid(a, b, c);
    if (is_equilateral(a, b, c))
        return flavor::equilateral;
    else if (is_isosceles(a, b, c))
        return flavor::isosceles;
    else
        return flavor::scalene;
    }

def is_valid_triangle(f):
    def inner(sides):
        return sum(sides) > 2 * max(sides) and f(len(set(sides)))

    return inner


@is_valid_triangle
def equilateral(number_of_sides):
    return number_of_sides == 1


@is_valid_triangle
def isosceles(number_of_sides):
    return number_of_sides < 3


@is_valid_triangle
def scalene(number_of_sides):
    return number_of_sides == 3

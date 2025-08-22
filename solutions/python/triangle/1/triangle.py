def is_valid_triangle(f):
    def inner(sides):
        return sum(sides) > 2 * max(sides) and f(sides)

    return inner


@is_valid_triangle
def equilateral(sides):
    return len(set(sides)) == 1


@is_valid_triangle
def isosceles(sides):
    return len(set(sides)) < 3


@is_valid_triangle
def scalene(sides):
    return len(set(sides)) == 3

from itertools import cycle


def spiral_matrix(size):
    matrix = [[None] * size for _ in range(size)]
    r_index, c_index = 0, 0
    deltas = cycle(((0, 1), (1, 0), (0, -1), (-1, 0)))
    delta_r, delta_c = next(deltas)
    for i in range(size**2):
        matrix[r_index][c_index] = i + 1
        if (
            not 0 <= r_index + delta_r < size
            or not 0 <= c_index + delta_c < size
            or matrix[r_index + delta_r][c_index + delta_c] is not None
        ):
            delta_r, delta_c = next(deltas)
        r_index += delta_r
        c_index += delta_c
    return matrix

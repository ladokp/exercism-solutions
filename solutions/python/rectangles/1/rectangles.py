from itertools import product


def rectangles(lines=None):
    if lines is None:
        return 0
    transposed = ["".join(line) for line in zip(*lines)]
    corners = []
    for i, row in enumerate(lines):
        for j, mark in enumerate(row):
            if mark == "+":
                corners.append((i, j))
    count = 0
    for (top, left), (bottom, right) in product(corners, repeat=2):
        if bottom <= top or right <= left:
            continue
        h = lines[top][left : right + 1] + lines[bottom][left : right + 1]
        v = (
            transposed[left][top : bottom + 1]
            + transposed[right][top : bottom + 1]
        )
        if " " not in h and "|" not in h and " " not in v and "-" not in v:
            count += 1
    return count

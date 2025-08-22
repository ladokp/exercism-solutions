def score(x, y):
    distance = (x**2 + y**2) ** 0.5
    hit_area = (
        distance <= 1,
        distance <= 5,
        distance <= 10,
    )
    match hit_area:
        case (True, *_):
            return 10
        case (_, True, _):
            return 5
        case (*_, True):
            return 1
    return 0

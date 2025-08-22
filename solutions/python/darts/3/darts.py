def score(x, y):
    distance, rings = (x**2 + y**2) ** 0.5, (1, 5, 10)
    for index, ring in enumerate(rings, start=1):
        if distance <= ring:
            return rings[-index]
    return 0

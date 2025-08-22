from itertools import permutations

ppl = "Norwegian Englishman Ukrainian Spaniard Japanese".split()


def drinks_water():
    return ppl[solve()[0]]


def owns_zebra():
    return ppl[solve()[1]]


def solve():
    return next(
        (water, zebra)
        for (red, green, ivory, yellow, blue) in permutations(range(5))
        if green - ivory == 1
        for (norway, english, ukraine, spain, japan) in permutations(range(5))
        if norway == 0
        if english == red
        for (dog, fox, snails, horse, zebra) in permutations(range(5))
        if spain == dog
        for (coffee, tea, milk, orange, water) in permutations(range(5))
        if coffee == green
        if ukraine == tea
        if milk == 2
        for (
            oldgold,
            kools,
            chesterfields,
            luckystrike,
            parliaments,
        ) in permutations(range(5))
        if oldgold == snails
        if kools == yellow
        if abs(chesterfields - fox) == 1
        if abs(kools - horse) == 1
        if luckystrike == orange
        if parliaments == japan
        if abs(norway - blue) == 1
    )

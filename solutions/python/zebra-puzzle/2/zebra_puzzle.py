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
        if norway == 0 and english == red
        for (dog, fox, snails, horse, zebra) in permutations(range(5))
        if spain == dog
        for (coffee, tea, milk, orange, water) in permutations(range(5))
        if all((coffee == green, ukraine == tea, milk == 2))
        for (
            oldgold,
            kools,
            chesterfields,
            luckystrike,
            parliaments,
        ) in permutations(range(5))
        if all(
            (
                oldgold == snails,
                kools == yellow,
                abs(chesterfields - fox) == 1,
                abs(kools - horse) == 1,
                luckystrike == orange,
                parliaments == japan,
                abs(norway - blue) == 1,
            )
        )
    )

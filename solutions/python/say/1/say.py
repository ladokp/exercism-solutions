ONES = lambda x: "zero one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen nineteen".split()[
    x
]
TENS = lambda x: "zero ten twenty thirty forty fifty sixty seventy eighty ninety".split()[
    x
]


def say(number):
    if not 0 <= number < 1e12:
        raise ValueError("input out of range")
    _, divisor, func, txt, hyphen = tuple(
        filter(
            lambda x: x[0],
            (
                (number < 20, 1, ONES, "", ""),
                (number < 1e2, 10, TENS, "", "-"),
                (number < 1e3, 100, ONES, " hundred", " "),
                (number < 1e6, 1_000, say, " thousand", " "),
                (number < 1e9, 1_000_000, say, " million", " "),
                (number < 1e12, 1_000_000_000, say, " billion", " "),
            ),
        )
    )[0]
    quotient, rest = divmod(number, divisor)
    return {0: lambda f, q, r, t, h: f(q) + t}.get(
        rest, lambda f, q, r, t, h: f(q) + t + h + say(r)
    )(func, quotient, rest, txt, hyphen)

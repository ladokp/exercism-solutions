from math import gcd


def measure(bucket_one, bucket_two, goal, start_bucket):
    if (
        goal <= 0
        or goal > max((bucket_one, bucket_two))
        or goal % gcd(bucket_one, bucket_two)
    ):
        raise ValueError("Invalid goal")
    if (goal, start_bucket) in ((bucket_one, "two"), (bucket_two, "one")):
        return 2, *(
            ("one", bucket_two)
            if start_bucket == "two"
            else ("two", bucket_one)
        )
    if start_bucket == "two":
        bucket_one, bucket_two = bucket_two, bucket_one
    one, two, turn = bucket_one, 0, 1
    while goal not in (one, two):
        one, two = (
            (one, 0)
            if two == bucket_two
            else (bucket_one, two)
            if one == 0
            else (max(0, one - (bucket_two - two)), min(bucket_two, one + two))
        )
        turn += 1
    if start_bucket == "two":
        one, two = two, one
    return turn, "one" if goal == one else "two", one if goal == two else two

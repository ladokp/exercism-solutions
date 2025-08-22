from datetime import timedelta

giga_second = timedelta(seconds=10**9)


def add(moment):
    return moment + giga_second

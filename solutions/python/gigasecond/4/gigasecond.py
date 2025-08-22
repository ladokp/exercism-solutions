from datetime import timedelta

giga_second = timedelta(seconds=1e9)


def add(moment):
    return moment + giga_second

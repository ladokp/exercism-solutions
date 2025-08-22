def append(list1, list2):
    return [*list1, *list2]


def concat(lists):
    concatenated_list = []
    for list_ in lists:
        concatenated_list = append(concatenated_list, list_)
    return concatenated_list


def filter(function, list_):
    return [item for item in list_ if function(item)]


def length(list_):
    return len(list_)


def map(function, list_):
    return [function(item) for item in list_]


def foldl(function, list_, initial):
    accumulator = initial
    for item in list_:
        accumulator = function(accumulator, item)
    return accumulator


def foldr(function, list_, initial):
    accumulator = initial
    for item in reverse(list_):
        accumulator = function(item, accumulator)
    return accumulator


def reverse(list_):
    return list_[::-1]

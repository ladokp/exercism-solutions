from itertools import groupby


def decode(code):
    count = ""
    decoded = ""
    for group_count, group in groupby(code):
        if group_count.isnumeric():
            count += str(group_count)
        elif not count:
            decoded += str(group_count)
        else:
            decoded += str(int(count) * group_count)
            count = ""
    return decoded


def encode(s):
    code = ""
    for group_count, group in groupby(s):
        section = sum(1 for _ in group)
        if section > 1:
            code += str(section) + group_count
        else:
            code += group_count
    return code

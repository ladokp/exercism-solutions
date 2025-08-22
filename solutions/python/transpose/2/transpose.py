import itertools


def transpose(lines):
    def custom_trim(line):
        return "".join(line).rstrip("~").replace("~", " ")

    transposed_list = [
        custom_trim(line)
        for line in itertools.zip_longest(*lines.splitlines(), fillvalue="~")
    ]
    return "\n".join(transposed_list)

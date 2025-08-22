import itertools


def transpose(lines):
    def trim(line):
        return "".join(line).rstrip("😁").replace("😁", " ")

    return "\n".join(
        trim(line)
        for line in itertools.zip_longest(*lines.splitlines(), fillvalue="😁")
    )

import itertools


def transpose(lines):
    return "\n".join(
        "".join(line).rstrip("😁").replace("😁", " ")
        for line in itertools.zip_longest(*lines.splitlines(), fillvalue="😁")
    )

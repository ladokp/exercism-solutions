import itertools

def transpose(lines):
    transposed_list = ["".join(line).rstrip("~").replace("~", " ") for line in itertools.zip_longest(*lines.splitlines(), fillvalue="~")]
    return "\n".join(transposed_list)

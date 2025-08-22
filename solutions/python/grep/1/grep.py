def grep(pattern, flags, files):
    (
        prepend_line_numbers,
        only_file_names,
        is_insensitive,
        invert_match,
        match_entire_line,
    ) = (
        "-n" in flags,
        "-l" in flags,
        "-i" in flags,
        "-v" in flags,
        "-x" in flags,
    )
    pattern = pattern.lower() if is_insensitive else pattern
    more_than_one_file = len(files) > 1
    result = ""
    for file in files:
        for line_number, line in enumerate(open(file).readlines()):
            line_insensitive = line.lower() if is_insensitive else line
            if invert_match ^ (
                not match_entire_line
                and pattern in line_insensitive
                or pattern + "\n" == line_insensitive
            ):
                if only_file_names:
                    result += file + "\n"
                    break
                result += (
                    (f"{file}:" if more_than_one_file else "")
                    + (f"{line_number+1}:" if prepend_line_numbers else "")
                    + line
                )
    return result

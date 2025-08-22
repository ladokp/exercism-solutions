"""
This module provides a function for searching text patterns (grep) in multiple files with various flags.
"""


def grep(pattern, flags, files):
    """
    Search for PATTERN in FILES. Flags can modify the behavior of the search.

    :param pattern: The text pattern to search for.
    :param flags: A list of flags that modify the search behavior.
        - "-n": Prepend line numbers to matching lines.
        - "-l": Output only the names of files with matching lines.
        - "-i": Perform case-insensitive matching.
        - "-v": Invert the sense of matching, to select non-matching lines.
        - "-x": Select only those matches that exactly match the whole line.
    :param files: A list of file names to search in.
    :return: A string containing the matched lines or file names.
    """
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
                    + (f"{line_number + 1}:" if prepend_line_numbers else "")
                    + line
                )
    return result

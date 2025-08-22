def proverb(*words, qualifier):
    lines = [
        f"For want of a {words[index]} the {words[index + 1]} was lost."
        for index in range(len(words) - 1)
    ]
    if words:
        lines.append(
            f"And all for the want of a {qualifier + ' ' if qualifier else ''}{words[0]}."
        )
    return lines

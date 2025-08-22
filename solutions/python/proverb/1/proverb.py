def proverb(*input_data, qualifier):
    rhyme_list = []
    if not input_data:
        return rhyme_list
    rhyme_length = len(input_data)
    for index, input_ in enumerate(input_data):
        if index == rhyme_length - 1:
            break
        rhyme_list.append(
            f"For want of a {input_} the {input_data[index+1]} was lost."
        )
    rhyme_list.append(
        f"And all for the want of a {qualifier + ' ' if qualifier else ''}{input_data[0]}."
    )
    return rhyme_list

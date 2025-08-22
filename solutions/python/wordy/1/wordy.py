import re

OPS = {
    "plus": "__add__",
    "minus": "__sub__",
    "multiplied": "__mul__",
    "divided": "__truediv__",
}


def answer(question):
    question = question.removeprefix("What is").removesuffix("?").strip()
    if not question:
        raise ValueError("syntax error")
    if question.isdigit():
        return int(question)
    solution = re.split(" by | ", question)
    if len(solution) == 2:
        message = "syntax error"
        if solution[1] not in OPS:
            message = "unknown operation"
        raise ValueError(message)
    while len(solution) > 1:
        try:
            operand_1, operator, operand_2, *tail = solution
            if operator not in OPS.keys() and not operator.isnumeric():
                raise ValueError("unknown operation")
            solution = [
                int(operand_1).__getattribute__(OPS[operator])(int(operand_2)),
                *tail,
            ]
        except Exception as e:
            if repr(e) == "ValueError('unknown operation')":
                raise e
            raise ValueError("syntax error")
    return solution[0]

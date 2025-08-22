CONDITIONS = ((3, "Pling"), (5, "Plang"), (7, "Plong"))

def convert(number):
    message_list = [message for condition, message in CONDITIONS if number % condition == 0]
    return "".join(message_list) or str(number)

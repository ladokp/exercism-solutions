def convert(number):
    message_list = []
    if number % 3 == 0:
        message_list.append("Pling")
    if number % 5 == 0:
        message_list.append("Plang")
    if number % 7 == 0:
        message_list.append("Plong")
    return "".join(message_list) or str(number)

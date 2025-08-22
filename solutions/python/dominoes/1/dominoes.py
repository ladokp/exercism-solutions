def can_chain(dominoes, chain=None):
    if chain is None:
        chain = []
    if not dominoes:
        return chain
    last_number = chain[-1][-1] if chain else dominoes[0][-1]
    for index, dominoes_ in enumerate(dominoes):
        if last_number in dominoes_:
            return_chain = can_chain(
                dominoes[:index] + dominoes[index + 1 :],
                chain
                + [
                    dominoes_
                    if dominoes_[0] == last_number
                    else dominoes_[::-1]
                ],
            )
            if return_chain and return_chain[0][0] == return_chain[-1][-1]:
                return return_chain

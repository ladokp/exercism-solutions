def saddle_points(matrix):
    if any(len(row) != len(matrix[0]) for row in matrix):
        raise ValueError("irregular matrix")
    row_max_indices = (
        (int(n), int(i))
        for n, k in enumerate(matrix)
        for i in range(len(k))
        if k[i] == max(k)
    )

    return [
        {"row": k[0] + 1, "column": k[1] + 1}
        for k in row_max_indices
        if matrix[k[0]][k[1]]
        == min(matrix[j][k[1]] for j in range(len(matrix)))
    ]

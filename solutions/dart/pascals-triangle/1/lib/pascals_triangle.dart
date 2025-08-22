class PascalsTriangle {
  List<List<int>> rows(int rows) {
    List<int> nextRow = [];

    final triangle = List.generate(rows, (rowIndex) {
      nextRow = [
        1,
        if (nextRow.length > 1)
          for (int index = 1; index < nextRow.length; index++) nextRow[index - 1] + nextRow[index],
        if (rowIndex > 0) 1,
      ];

      return nextRow;
    });

    return triangle;
  }
}
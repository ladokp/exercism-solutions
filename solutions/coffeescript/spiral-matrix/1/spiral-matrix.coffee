class SpiralMatrix
  @spiralMatrix: (size) ->
    matrix = Array(size * size).fill 0
    currentX = 0
    stopX = size - 1
    currentY = 0
    stopY = size - 1
    currentNumber = 1

    while currentX < stopX and currentY <= stopY
      for n in [currentX..stopX]
        matrix[currentY * size + n] = currentNumber++
      currentY++

      for n in [currentY..stopY]
        matrix[n * size + stopX] = currentNumber++
      stopX--

      if currentY == stopY
        break

      for n in [stopX..currentX] by -1
        matrix[stopY * size + n] = currentNumber++
      stopY--

      for n in [stopY..currentY] by -1
        matrix[n * size + currentX] = currentNumber++
      currentX++

    matrix[currentY * size + currentX] = currentNumber

    result = []
    for i in [0...size]
      result.push matrix.slice(i * size, (i + 1) * size)

    result

module.exports = SpiralMatrix

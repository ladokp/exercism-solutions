package rectangles

type Point struct{ x, y uint16 }

func Count(matrix []string) (count int) {
	if len(matrix) == 0 || len(matrix[0]) == 0 {
		return
	}

	vertices := make([]Point, 0, (len(matrix)*len(matrix[0]))/2)
	for y := range matrix {
		for x := range []byte(matrix[y]) {
			if matrix[y][x] == '+' {
				vertices = append(vertices, Point{uint16(x), uint16(y)})
			}
		}
	}

	for index, point := range vertices {
	outerLoop:
		for _, currentPoint := range vertices[index+1:] {
			if point.y >= currentPoint.y || point.x >= currentPoint.x || matrix[point.y][currentPoint.x] != '+' || matrix[currentPoint.y][point.x] != '+' {
				continue
			}
			for x := point.x + 1; x < currentPoint.x; x++ {
				if (matrix[point.y][x] != '-' && matrix[point.y][x] != '+') || (matrix[currentPoint.y][x] != '-' && matrix[currentPoint.y][x] != '+') {
					continue outerLoop
				}
			}
			for y := point.y + 1; y < currentPoint.y; y++ {
				if (matrix[y][point.x] != '|' && matrix[y][point.x] != '+') || (matrix[y][currentPoint.x] != '|' && matrix[y][currentPoint.x] != '+') {
					continue outerLoop
				}
			}
			count++
		}
	}
	return
}

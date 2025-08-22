package pascal

func Triangle(depth int) (triangle [][]int) {
	for row := 0; row < depth; row++ {
		line := []int{1}
		for position := 1; position <= row; position++ {
			line = append(line, line[position-1]*(row+1-position)/position)
		}
		triangle = append(triangle, line)
	}
	return
}

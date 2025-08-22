package grep

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strings"
)

func Search(pattern string, flags, files []string) (result []string) {
	if slices.Contains(flags, "-i") {
		pattern = strings.ToLower(pattern)
	}

	for _, path := range files {
		file, err := os.Open(path)
		if err != nil {
			panic(err)
		}
		defer file.Close()

		fileCount := len(files)
		scanner := bufio.NewScanner(file)
		lineNumber := 0
		for scanner.Scan() {
			lineNumber++
			line := scanner.Text()
			check := line
			if slices.Contains(flags, "-i") {
				check = strings.ToLower(check)
			}

			isExactSearch := slices.Contains(flags, "-x")
			isInvertedSearch := slices.Contains(flags, "-v")
			isPositiveMatch := isExactSearch && check == pattern || !isExactSearch && strings.Contains(check, pattern)
			if !isInvertedSearch && isPositiveMatch || isInvertedSearch && !isPositiveMatch {
				fileNameSuffix := ""
				if fileCount > 1 {
					fileNameSuffix = fmt.Sprintf("%s:", path)
				}
				if slices.Contains(flags, "-l") {
					result = append(result, path)
					break
				}
				if slices.Contains(flags, "-n") {
					line = fmt.Sprintf("%d:%s", lineNumber, line)
				}
				result = append(result, fmt.Sprintf("%s%s", fileNameSuffix, line))
			}
		}

		if err := scanner.Err(); err != nil {
			log.Fatal(err)
		}
	}
	return
}

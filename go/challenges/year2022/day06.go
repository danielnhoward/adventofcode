package year2022

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day06(input string) [2]any {
	splitInput := strings.Split(input, "")
	partOne := findInputMarkerLocation(splitInput, 4)
	partTwo := findInputMarkerLocation(splitInput, 14)

	return [2]any{partOne, partTwo}
}

func findInputMarkerLocation(input []string, count int) int {
	for i := 0; i < len(input)-count; i++ {
		if utils.ArrayValuesAreUnique(input[i : i+count+1]) {
			return i + count
		}
	}

	return -1
}

package year2015

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day02(input string) [2]any {
	splitInput := strings.Split(input, "\r\n")
	totalArea := 0
	totalRibbon := 0

	for _, sizeStr := range splitInput {
		dimensions := utils.SplitStrToInt[int](sizeStr, "x")

		area, ribbon := getSqaureArea(dimensions...)
		totalArea += area
		totalRibbon += ribbon
	}

	return [2]any{totalArea, totalRibbon}
}

func getSqaureArea(dimensions ...int) (area int, ribbon int) {
	area += dimensions[0] * dimensions[1]
	area += dimensions[1] * dimensions[2]
	area += dimensions[0] * dimensions[2]
	area *= 2

	// Get smallest side to add area for slack
	smallSides := utils.ArrayGetBoundValues(dimensions, 2, true)
	area += smallSides[0] * smallSides[1]

	ribbon += dimensions[0] * dimensions[1] * dimensions[2]
	ribbon += 2 * (smallSides[0] + smallSides[1])

	return
}

package year2022

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day04(input string) [2]any {
	elfData := strings.Split(input, "\r\n")

	fullOverlapCount := findElfOverlapCount(elfData, true)
	overlapCount := findElfOverlapCount(elfData, false)

	return [2]any{fullOverlapCount, overlapCount}
}

func findElfOverlapCount(data []string, isFullOverlap bool) (count int) {
	for _, twoElvesData := range data {
		ranges := strings.Split(twoElvesData, ",")

		elfOneRange := utils.SplitStrToInt[int](ranges[0], "-")
		elfTwoRange := utils.SplitStrToInt[int](ranges[1], "-")

		query := false
		if isFullOverlap {
			query = (elfOneRange[0] >= elfTwoRange[0] && elfOneRange[1] <= elfTwoRange[1]) || (elfTwoRange[0] >= elfOneRange[0] && elfTwoRange[1] <= elfOneRange[1])
		} else {
			elfOneCoverValues := []int{}
			for i := elfOneRange[0]; i <= elfOneRange[1]; i++ {
				elfOneCoverValues = append(elfOneCoverValues, i)
			}

			for i := elfTwoRange[0]; i <= elfTwoRange[1]; i++ {
				if utils.ArrayContains(elfOneCoverValues, i) {
					query = true
					break
				}
			}
		}

		if query {
			count++
		}
	}
	return
}

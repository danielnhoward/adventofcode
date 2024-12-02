package year2022

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day08(input string) [2]any {
	splitInput := strings.Split(input, "\r\n")
	grid := processTreeGrid(splitInput)

	partOneVisibleCount := getVisibleTrees(grid)
	partTwoVisibleCount := getBestVisibilityScore(grid)

	return [2]any{partOneVisibleCount, partTwoVisibleCount}
}

func processTreeGrid(input []string) (grid [][]int) {
	for _, rowData := range input {
		grid = append(grid, utils.SplitStrToInt[int](rowData, ""))
	}

	return
}

func getVisibleTrees(grid [][]int) (count int) {
	for x, row := range grid {
		for y, tree := range row {
			treeIsVisible := false

			for _, direction := range utils.Directions {
				location := utils.Coordinate{
					X: x,
					Y: y,
				}

				for {
					location.X += direction[0]
					location.Y += direction[1]

					if location.X < 0 || location.X >= len(row) || location.Y < 0 || location.Y >= len(grid) {
						treeIsVisible = true
						break
					}

					targetTree := grid[location.X][location.Y]
					if tree <= targetTree {
						break
					}
				}

				if treeIsVisible {
					count++
					break
				}
			}
		}
	}

	return
}

func getBestVisibilityScore(grid [][]int) int {
	totalScores := []int{}
	for x, row := range grid {
		for y, tree := range row {
			scores := []int{}

			for _, direction := range utils.Directions {
				treesViewable := 0
				location := utils.Coordinate{
					X: x,
					Y: y,
				}

				for {
					location.X += direction[0]
					location.Y += direction[1]

					if location.X < 0 || location.X >= len(row) || location.Y < 0 || location.Y >= len(grid) {
						break
					}

					treesViewable++

					targetTree := grid[location.X][location.Y]
					if targetTree >= tree {
						break
					}
				}

				scores = append(scores, treesViewable)
			}

			score := utils.ArrayMultiply(scores)
			totalScores = append(totalScores, score)
		}
	}

	return utils.ArrayGetBoundValues(totalScores, 1, false)[0]
}

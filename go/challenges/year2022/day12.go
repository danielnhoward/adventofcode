package year2022

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
	"github.com/Toffee1347/adventofcode/utils/dijkstra"
)

func Day12(input string) [2]any {
	elevationMap, startPoint, endPoint, aPoints := processHeatmapData(strings.Split(input, "\r\n"))

	partOneValue := findBestRouteLength(elevationMap, []utils.Coordinate{startPoint}, endPoint)
	partTwoValue := findBestRouteLength(elevationMap, aPoints, endPoint)

	return [2]any{partOneValue, partTwoValue}
}

func processHeatmapData(rows []string) (elevation [][]int, startPoint utils.Coordinate, endPoint utils.Coordinate, aPoints []utils.Coordinate) {
	for y, row := range rows {
		elevation = append(elevation, []int{})
		for x := 0; x < len(row); x++ {
			letter := row[x]
			value := int(letter) - 97
			coord := utils.Coordinate{X: x, Y: y}

			if letter == 'S' {
				startPoint = coord
				value = 0
			} else if letter == 'E' {
				endPoint = coord
				value = 25
			}

			if letter == 'S' || letter == 'a' {
				aPoints = append(aPoints, coord)
			}

			elevation[y] = append(elevation[y], value)
		}
	}

	return
}

func findBestRouteLength(elevationMap [][]int, startPoints []utils.Coordinate, endPoint utils.Coordinate) int {
	jobQueue := [][]int{}
	for _, startPoint := range startPoints {
		job := append([]int{0}, utils.AxisConvertCoordinateToSlice(startPoint)...)
		jobQueue = append(jobQueue, job)
	}

	visited := map[string]bool{}

	for len(jobQueue) != 0 {
		job, newQueue := dijkstra.GetNextJob(jobQueue)
		jobQueue = newQueue

		length := job[0]
		point := utils.AxisConvertSliceToCoordinate(job[1:])
		elevation := elevationMap[point.Y][point.X]

		if utils.AxisCoordinatesMatch(point, endPoint) {
			return length
		}

		visitedKey := utils.IntToStr(point.X) + ":" + utils.IntToStr(point.Y)
		if visited[visitedKey] {
			continue
		}
		visited[visitedKey] = true

		for _, direction := range utils.Directions {
			newPoint := utils.Coordinate{
				X: point.X + direction[0],
				Y: point.Y + direction[1],
			}

			if newPoint.X < 0 || newPoint.X >= len(elevationMap[0]) || newPoint.Y < 0 || newPoint.Y >= len(elevationMap) {
				continue
			}

			newElevation := elevationMap[newPoint.Y][newPoint.X]
			if newElevation <= elevation+1 {
				newJob := append([]int{length + 1}, utils.AxisConvertCoordinateToSlice(newPoint)...)
				jobQueue = append(jobQueue, newJob)
			}
		}
	}

	return -1
}

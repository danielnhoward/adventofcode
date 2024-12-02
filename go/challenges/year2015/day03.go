package year2015

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

var directions map[string]utils.Coordinate = map[string]utils.Coordinate{
	"^": {X: 0, Y: 1},
	">": {X: 1, Y: 0},
	"v": {X: 0, Y: -1},
	"<": {X: -1, Y: 0},
}

func Day03(input string) [2]any {
	inputDirections := strings.Split(input, "")

	oneTrip, _ := visitedLocationCount(inputDirections, map[string]bool{})

	splitDirections := []([]string){
		[]string{},
		[]string{},
	}

	for i := 0; i < len(inputDirections); i++ {
		splitDirections[i%2] = append(splitDirections[i%2], inputDirections[i])
	}

	_, locations := visitedLocationCount(splitDirections[0], map[string]bool{})
	count, _ := visitedLocationCount(splitDirections[1], locations)

	return [2]any{oneTrip, count}
}

func visitedLocationCount(inputDirections []string, visitedLocations map[string]bool) (int, map[string]bool) {
	currentLocation := utils.Coordinate{X: 0, Y: 0}
	if len(visitedLocations) == 0 {
		visitedLocations = map[string]bool{
			"0:0": true,
		}
	}

	for _, direction := range inputDirections {
		currentLocation = modifyCoordinate(currentLocation, directions[direction])

		visitedLocations[getCoordinateString(currentLocation)] = true
	}

	return len(visitedLocations), visitedLocations
}

func modifyCoordinate(orig utils.Coordinate, modifier utils.Coordinate) utils.Coordinate {
	orig.X += modifier.X
	orig.Y += modifier.Y
	return orig
}

func getCoordinateString(coord utils.Coordinate) string {
	x := utils.IntToStr(coord.X)
	y := utils.IntToStr(coord.Y)

	return x + ":" + y
}

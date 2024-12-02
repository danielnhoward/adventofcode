package year2015

import (
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day06(input string) [2]any {
	instructions := strings.Split(input, "\r\n")

	onOffGrid := [1000]([1000]bool){}
	brightnessGrid := [1000]([1000]int){}

	for _, instuction := range instructions {
		bounds := getLightCoordinates(instuction)

		for x := bounds[0].X; x <= bounds[1].X; x++ {
			for y := bounds[0].Y; y <= bounds[1].Y; y++ {
				if strings.HasPrefix(instuction, "turn on") {
					onOffGrid[x][y] = true
					brightnessGrid[x][y]++
				} else if strings.HasPrefix(instuction, "turn off") {
					onOffGrid[x][y] = false
					if brightnessGrid[x][y] > 0 {
						brightnessGrid[x][y]--
					}
				} else if strings.HasPrefix(instuction, "toggle") {
					onOffGrid[x][y] = !onOffGrid[x][y]
					brightnessGrid[x][y] += 2
				}
			}
		}
	}

	onCount := 0
	for _, row := range onOffGrid {
		for _, part := range row {
			if part {
				onCount++
			}
		}
	}

	brightnessCount := 0
	for _, row := range brightnessGrid {
		for _, part := range row {
			brightnessCount += part
		}
	}

	return [2]any{onCount, brightnessCount}
}

func getLightCoordinates(input string) (coords [2]utils.Coordinate) {
	regex := regexp.MustCompile(`\d+,\d+`)
	coordsStrings := regex.FindAllString(input, -1)

	if len(coordsStrings) != 2 {
		fmt.Println("getLightCoordinates requires there to be two coords in the input")
		os.Exit(1)
	}

	for i, coordString := range coordsStrings {
		parts := utils.SplitStrToInt[int](coordString, ",")

		coords[i] = utils.Coordinate{X: parts[0], Y: parts[1]}
	}

	return
}

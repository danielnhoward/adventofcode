package year2022

import (
	"math"
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day10(input string) [2]any {
	instructions := strings.Split(input, "\r\n")
	values := processCpuInstructions(instructions)

	partOneValue := 0
	for i := 20; i <= 220; i += 40 {
		partOneValue += values[i-1] * i
	}

	partTwoValue := generateImageGrid(values)

	return [2]any{partOneValue, "\n" + partTwoValue}
}

func processCpuInstructions(instructions []string) (values []int) {
	currentX := 1
	for _, instruction := range instructions {
		values = append(values, currentX)

		if strings.HasPrefix(instruction, "addx ") {
			values = append(values, currentX)

			value := utils.StrToInt[int](strings.ReplaceAll(instruction, "addx ", ""))
			currentX += value
		}
	}

	return
}

func generateImageGrid(values []int) string {
	valuesStore := []string{}
	row := ""

	for i, value := range values {
		currentPixel := i % 40
		if math.Abs(float64(currentPixel-value)) <= 1 {
			row += "#"
		} else {
			row += "."
		}

		if i%40 == 39 {
			valuesStore = append(valuesStore, row)
			row = ""
		}
	}

	return strings.Join(valuesStore, "\n")
}

package year2022

import (
	"regexp"
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

type supplyColumn map[int]([]string)

func Day05(input string) [2]any {
	splitInput := strings.Split(input, "\r\n\r\n")

	instructions := strings.Split(splitInput[1], "\r\n")
	partOne := processCraneInstruction(instructions, processInitialColumns(splitInput[0]), false)
	partTwo := processCraneInstruction(instructions, processInitialColumns(splitInput[0]), true)

	return [2]any{partOne, partTwo}
}

func processCraneInstruction(instructions []string, columns supplyColumn, canCarryMultiple bool) string {
	instructionRegex := regexp.MustCompile(`move (\d+) from (\d+) to (\d+)`)

	for _, instruction := range instructions {
		instructionStringData := instructionRegex.FindStringSubmatch(instruction)

		instructionData := []int{}
		for i := 1; i < len(instructionStringData); i++ {
			instructionData = append(instructionData, utils.StrToInt[int](instructionStringData[i]))
		}

		moveCount := instructionData[0]
		moveFromI := instructionData[1] - 1
		moveToI := instructionData[2] - 1

		if canCarryMultiple {
			moveColumnLength := len(columns[moveFromI])
			columns[moveToI] = append(columns[moveToI], columns[moveFromI][moveColumnLength-moveCount:]...)
			columns[moveFromI] = columns[moveFromI][0 : moveColumnLength-moveCount]
		} else {
			for i := 0; i < moveCount; i++ {
				moveIndex := len(columns[moveFromI]) - 1
				columns[moveToI] = append(columns[moveToI], columns[moveFromI][moveIndex])
				columns[moveFromI] = columns[moveFromI][0:moveIndex]
			}
		}
	}

	output := ""
	for columnI := 0; columnI < len(columns); columnI++ {
		column := columns[columnI]
		if len(column) == 0 {
			output += " "
		} else {
			output += string(column[len(column)-1])
		}
	}

	return output
}

func processInitialColumns(inputColumns string) (columns supplyColumn) {
	columns = supplyColumn{}
	rows := strings.Split(inputColumns, "\r\n")

	for i := len(rows) - 2; i >= 0; i-- {
		row := rows[i]

		returnColumnI := 0
		for columnI := 1; columnI <= len(row)-2; columnI += 4 {
			cellValue := string(row[columnI])
			if cellValue != " " {
				columns[returnColumnI] = append(columns[returnColumnI], cellValue)
			}

			returnColumnI++
		}
	}

	return
}

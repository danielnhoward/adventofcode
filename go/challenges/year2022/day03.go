package year2022

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day03(input string) [2]any {
	rucksacks := strings.Split(input, "\r\n")

	partOneTotal := 0
	for _, rucksack := range rucksacks {
		size := len(rucksack)
		firstComp := rucksack[0 : size/2]
		secondComp := rucksack[size/2 : size]

		common := utils.StringCommon(firstComp, secondComp)[0]
		partOneTotal += getLetterValue(common)
	}

	partTwoTotal := 0
	for i := 0; i < len(rucksacks); i += 3 {
		common := utils.StringCommon(rucksacks[i], rucksacks[i+1:i+3]...)[0]
		partTwoTotal += getLetterValue(common)
	}

	return [2]any{partOneTotal, partTwoTotal}
}

func getLetterValue(letter rune) int {
	ascii := int(letter) - 96
	if ascii <= 0 {
		ascii += 58
	}
	return ascii
}

package year2015

import (
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day05(input string) [2]any {
	people := strings.Split(input, "\r\n")

	partOneNiceCount := 0
	partTwoNiceCount := 0
	for _, person := range people {
		if partOneStringIsNice(person) {
			partOneNiceCount++
		}
		if partTwoStringIsNice(person) {
			partTwoNiceCount++
		}
	}

	return [2]any{partOneNiceCount, partTwoNiceCount}
}

var vowels string = "aeiou"
var invalidStrings []string = []string{"ab", "cd", "pq", "xy"}

func partOneStringIsNice(input string) bool {
	commonParts := utils.StringCommon(input, vowels)
	if len(commonParts) < 3 {
		return false
	}

	repeatedLetterExists := false
	for i := 0; i < len(input)-1; i++ {
		if input[i] == input[i+1] {
			repeatedLetterExists = true
		}
	}
	if !repeatedLetterExists {
		return false
	}

	for _, invalidString := range invalidStrings {
		if strings.Contains(input, invalidString) {
			return false
		}
	}

	return true
}

func partTwoStringIsNice(input string) bool {
	letterCombinationSets := map[string]int{}
	containsDoubleLetter := false
	for i := 0; i < len(input)-1; i++ {
		combination := input[i : i+2]

		combinationLocation, exists := letterCombinationSets[combination]
		if exists && combinationLocation != i-1 {
			containsDoubleLetter = true
			break
		}

		if !exists {
			letterCombinationSets[combination] = i
		}
	}
	if !containsDoubleLetter {
		return false
	}

	for i := 0; i < len(input)-2; i++ {
		if input[i] == input[i+2] {
			return true
		}
	}

	return false
}

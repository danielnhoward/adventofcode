package year2022

import (
	"fmt"
	"os"
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

type monkeyInput struct {
	Items            []int64
	StartingItemsRaw string `yaml:"Starting items"`
	Operation        string `yaml:"Operation"`
	Test             string `yaml:"Test"`
	TestTrue         string `yaml:"If true"`
	TestFalse        string `yaml:"If false"`
}

func Day11(input string) [2]any {
	monkeys := processMonkeyGameInput(strings.ReplaceAll(input, "  If ", "If "))

	// Part 1
	partOneInspectCount := getMonkeyInspectCount(monkeys, 20, 3)
	partOneTwoBiggest := utils.ArrayGetBoundValues(partOneInspectCount, 2, false)
	partOneCount := utils.ArrayMultiply(partOneTwoBiggest)

	// Part 2
	monkeys = processMonkeyGameInput(strings.ReplaceAll(input, "  If ", "If "))
	partTwoInspectCount := getMonkeyInspectCount(monkeys, 10000, 1)
	partTwoTwoBiggest := utils.ArrayGetBoundValues(partTwoInspectCount, 2, false)
	partTwoCount := utils.ArrayMultiply(partTwoTwoBiggest)

	return [2]any{partOneCount, partTwoCount}
}

func processMonkeyGameInput(input string) []monkeyInput {
	monkeys := utils.EncodingParseYaml[map[string]monkeyInput](input)

	for name, monkey := range monkeys {
		monkey.Items = utils.SplitStrToInt[int64](monkey.StartingItemsRaw, ", ")
		monkeys[name] = monkey
	}

	biggestMonkeyN := 0
	for name := range monkeys {
		name = strings.ReplaceAll(name, "Monkey ", "")

		number := utils.StrToInt[int](name)
		if number > biggestMonkeyN {
			biggestMonkeyN = number
		}
	}

	monkeysSlice := []monkeyInput{}
	for i := 0; i <= biggestMonkeyN; i++ {
		monkeysSlice = append(monkeysSlice, monkeys["Monkey "+utils.IntToStr(i)])
	}

	return monkeysSlice
}

func getMonkeyInspectCount(monkeys []monkeyInput, goesCount int, worryLevel int64) (inspectionCount []int) {
	inspectionCount = []int{}

	for i := 0; i < len(monkeys); i++ {
		inspectionCount = append(inspectionCount, 0)
	}

	var divisor int64 = 1
	for _, monkey := range monkeys {
		divisor *= getDivisibleValue(monkey.Test)
	}

	for goI := 0; goI < goesCount; goI++ {
		for monkeyI := 0; monkeyI < len(monkeys); monkeyI++ {
			monkey := monkeys[monkeyI]

			if len(monkey.Items) == 0 {
				continue
			}

			inspectionCount[monkeyI] += len(monkey.Items)

			for {
				currentItem := monkey.Items[0]
				currentItem = completeOperation(monkey.Operation, currentItem)
				currentItem /= worryLevel
				currentItem %= divisor

				if currentItem%getDivisibleValue(monkey.Test) == 0 {
					monkeys = executeMonkeyTestResult(monkey.TestTrue, monkeys, currentItem)
				} else {
					monkeys = executeMonkeyTestResult(monkey.TestFalse, monkeys, currentItem)
				}

				monkey.Items = monkey.Items[1:]
				monkeys[monkeyI] = monkey

				if len(monkey.Items) == 0 {
					break
				}
			}
		}
	}

	return
}

func completeOperation(operation string, old int64) int64 {
	operationValues := utils.GetRegexGroups(`new = old ([\/\*\-\+]) (\d+|old)`, operation)

	a := old
	b := old
	if operationValues[1] != "old" {
		b = utils.StrToInt[int64](operationValues[1])
	}

	if operationValues[0] == `/` {
		return a / b
	} else if operationValues[0] == `*` {
		return a * b
	} else if operationValues[0] == `-` {
		return a - b
	} else if operationValues[0] == `+` {
		return a + b
	}

	fmt.Println("No operation found in " + operation)
	os.Exit(1)
	return 0
}

func getDivisibleValue(test string) int64 {
	testValueStr := utils.GetRegexGroups(`divisible by (\d+)`, test)[0]
	return utils.StrToInt[int64](testValueStr)
}

func executeMonkeyTestResult(result string, monkeys []monkeyInput, item int64) []monkeyInput {
	throwTo := utils.GetRegexGroups(`throw to monkey (\d+)`, result)[0]
	throwToInt := utils.StrToInt[int64](throwTo)

	targetMonkey := monkeys[throwToInt]
	targetMonkey.Items = append(targetMonkey.Items, item)
	monkeys[throwToInt] = targetMonkey

	return monkeys
}

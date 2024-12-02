package year2015

import (
	"regexp"
	"strconv"
	"strings"
)

func Day07(input string) [2]any {
	instructions := strings.Split(input, "\r\n")

	partOneValue := calculateWireValue("a", instructions, 0)
	partTwoValue := calculateWireValue("a", instructions, partOneValue)

	return [2]any{int(partOneValue), int(partTwoValue)}
}

func calculateWireValue(target string, instructions []string, bValue uint16) uint16 {
	effecterRegex := regexp.MustCompile(`-> ([a-z0-9]+)`)

	regexes := map[string]*regexp.Regexp{
		"AND":    regexp.MustCompile(`([a-z0-9]+) AND ([a-z0-9]+)`),
		"OR":     regexp.MustCompile(`([a-z0-9]+) OR ([a-z0-9]+)`),
		"LSHIFT": regexp.MustCompile(`([a-z0-9]+) LSHIFT ([a-z0-9]+)`),
		"RSHIFT": regexp.MustCompile(`([a-z0-9]+) RSHIFT ([a-z0-9]+)`),
		"NOT":    regexp.MustCompile(`NOT ([a-z0-9]+)`),
	}
	valueSetRegex := regexp.MustCompile(`([a-z0-9]+)`)

	wireValues := map[string]uint16{}

	for {
		if _, exists := wireValues[target]; exists {
			break
		}

		for _, instruction := range instructions {
			actionEffecter := effecterRegex.FindStringSubmatch(instruction)[1]

			variables := []uint16{}
			exists := false
			commandFound := false
			for command, regex := range regexes {
				if strings.Contains(instruction, command) {
					variables, exists = getInputValues(regex, instruction, wireValues)
					commandFound = true
				}
			}
			if !commandFound {
				variables, exists = getInputValues(valueSetRegex, instruction, wireValues)
			}

			if !exists {
				continue
			}

			if strings.Contains(instruction, "AND") {
				wireValues[actionEffecter] = variables[0] & variables[1]
			} else if strings.Contains(instruction, "OR") {
				wireValues[actionEffecter] = variables[0] | variables[1]
			} else if strings.Contains(instruction, "LSHIFT") {
				wireValues[actionEffecter] = variables[0] << variables[1]
			} else if strings.Contains(instruction, "RSHIFT") {
				wireValues[actionEffecter] = variables[0] >> variables[1]
			} else if strings.Contains(instruction, "NOT") {
				wireValues[actionEffecter] = ^variables[0]
			} else {
				if actionEffecter == "b" && bValue != 0 {
					wireValues[actionEffecter] = bValue
				} else {
					wireValues[actionEffecter] = variables[0]
				}
			}
		}
	}

	return wireValues[target]
}

func getInputValues(variablesRegex *regexp.Regexp, input string, currentWiresmap map[string]uint16) (values []uint16, valuesExist bool) {
	inputValues := variablesRegex.FindStringSubmatch(input)[1:]

	for _, value := range inputValues {
		numberValue, err := strconv.ParseUint(value, 10, 16)
		if err == nil {
			values = append(values, uint16(numberValue))
		} else {
			wireValue, exists := currentWiresmap[value]
			if !exists {
				return []uint16{}, false
			}
			values = append(values, wireValue)
		}
	}

	valuesExist = true
	return
}

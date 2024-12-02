package year2015

func Day01(input string) [2]any {
	upCount := getCharacterCount(input, "(")
	downCount := getCharacterCount(input, ")")
	finalFloor := upCount - downCount

	pointAtBasement := getIndexInBasement(input)

	return [2]any{finalFloor, pointAtBasement}
}

func getCharacterCount(src string, targetString string) (count int) {
	for i := 0; i < len(src); i++ {
		if string(src[i]) == targetString {
			count++
		}
	}
	return
}

func getIndexInBasement(src string) int {
	currentFloor := 0

	for i := 0; i < len(src); i++ {
		action := string(src[i])
		if action == "(" {
			currentFloor++
		} else if action == ")" {
			currentFloor -= 1
		}

		if currentFloor == -1 {
			return i + 1
		}
	}

	return -1
}

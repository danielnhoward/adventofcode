package year2015

import (
	"crypto/md5"
	"encoding/hex"

	"github.com/Toffee1347/adventofcode/utils"
)

func Day04(input string) [2]any {
	partOneValue := 0
	partTwoValue := 0

	for i := 0; true; i++ {
		hash := md5.Sum([]byte(input + utils.IntToStr(i)))
		hashString := hex.EncodeToString(hash[:])

		if hashString[0:5] == "00000" && partOneValue == 0 {
			partOneValue = i
		}

		if hashString[0:6] == "000000" && partTwoValue == 0 {
			partTwoValue = i
		}

		if partOneValue != 0 && partTwoValue != 0 {
			break
		}
	}

	return [2]any{partOneValue, partTwoValue}
}

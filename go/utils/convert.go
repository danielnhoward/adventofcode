package utils

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"golang.org/x/exp/constraints"
)

func StrToInt[T constraints.Integer](src string) T {
	convInt, err := strconv.ParseInt(src, 10, 64)
	if err != nil {
		fmt.Println("Failed to convert ", src, " to an int ", err.Error())
		os.Exit(1)
	}

	return T(convInt)
}

func StrToUint(src string) uint {
	convInt, err := strconv.ParseUint(src, 10, 64)
	if err != nil {
		fmt.Println("Failed to convert ", src, " to an int ", err.Error())
		os.Exit(1)
	}

	return uint(convInt)
}

func SplitStrToInt[T constraints.Integer](data string, sep string) (newData []T) {
	splitData := strings.Split(data, sep)

	for _, singleData := range splitData {
		newData = append(newData, StrToInt[T](singleData))
	}

	return
}

func IntToStr(src int) string {
	return strconv.FormatInt(int64(src), 10)
}

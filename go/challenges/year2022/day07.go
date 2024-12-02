package year2022

import (
	"fmt"
	"os"
	"strings"

	"github.com/Toffee1347/adventofcode/utils"
)

type directory struct {
	Directoies map[string]directory
	Files      []file
	Size       int
}

type file struct {
	Name string
	Size int
}

func Day07(input string) [2]any {
	splitInput := strings.Split(input, "\r\n")

	files, _ := processCommandLineInput(splitInput)
	files = findDirectoriesSize(files)

	dirSizes := getAllDirSizes(files)
	partOneTotal := utils.ArraySum(utils.ArrayGetValuesLessThan(dirSizes, 100000))

	spaceFree := 70000000 - files.Size
	spaceNeeded := 30000000 - spaceFree
	orderedSizes := utils.ArrayGetBoundValues(dirSizes, -1, true)

	partTwoValue := 0
	for _, value := range orderedSizes {
		if value >= spaceNeeded {
			partTwoValue = value
			break
		}
	}

	return [2]any{partOneTotal, partTwoValue}
}

func processCommandLineInput(input []string) (currentDirectory directory, newInput []string) {
	currentDirectory = directory{
		Directoies: map[string]directory{},
	}

	for {
		if !strings.HasPrefix(input[0], "$ ") {
			fmt.Println(input[0], "isn't a command")
			os.Exit(1)
		}

		command := strings.Split(input[0], " ")[1:]
		input = input[1:]
		if command[0] == "cd" {
			newDirectory := command[1]

			if newDirectory == ".." {
				newInput = input
				return
			} else {
				currentDirectory.Directoies[newDirectory], input = processCommandLineInput(input)

				if len(input) == 0 {
					return
				}
			}
		} else if command[0] == "ls" {
			for i, value := range input {
				if strings.HasPrefix(value, "$ ") {
					input = input[i:]
					break
				}

				splitValue := strings.Split(value, " ")
				if splitValue[0] != "dir" {
					file := file{
						Name: splitValue[1],
						Size: utils.StrToInt[int](splitValue[0]),
					}

					currentDirectory.Files = append(currentDirectory.Files, file)
				}

				if i == len(input)-1 {
					newInput = []string{}
					return
				}
			}
		}
	}
}

func findDirectoriesSize(files directory) (newFiles directory) {
	newFiles = directory{
		Directoies: map[string]directory{},
		Files:      files.Files,
	}

	currentDirSize := 0
	for _, file := range files.Files {
		currentDirSize += file.Size
	}

	for key, innerDirectory := range files.Directoies {
		newFiles.Directoies[key] = findDirectoriesSize(innerDirectory)
		currentDirSize += newFiles.Directoies[key].Size
	}

	newFiles.Size = currentDirSize
	return
}

func getAllDirSizes(files directory) (sizes []int) {
	sizes = append(sizes, files.Size)

	for _, dir := range files.Directoies {
		sizes = append(sizes, getAllDirSizes(dir)...)
	}

	return
}

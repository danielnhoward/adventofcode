package main

import (
	"flag"
	"fmt"
	"os"
)

func main() {
	var year int
	flag.IntVar(&year, "year", 2022, "The year of the challenge to run")

	var day int
	flag.IntVar(&day, "day", 1, "The day of the challenge to run")

	var useExampleInput bool
	flag.BoolVar(&useExampleInput, "example", false, "Whether to use example data or not")

	flag.Parse()

	_, exists := challenges[year]
	if !exists {
		fmt.Println("The challenges during", year, "could not be found")
		os.Exit(1)
	}

	handler, exists := challenges[year][day]
	if !exists {
		fmt.Println("The challenges during year", year, "on day", day, "could not be found")
		os.Exit(1)
	}

	input, err := getInput(year, day, useExampleInput)
	if err != nil {
		extraMessage := ""
		if useExampleInput {
			extraMessage = "There may not be an input avaliable for the example"
		}
		fmt.Println("An input could not be located for year", year, "day", day, extraMessage)
		os.Exit(1)
	}

	data := handler(input)
	fmt.Println("Data for Advent of Code", year, "day", day, "has successfully been calculated")
	fmt.Println("Part 1:", data[0])
	fmt.Println("Part 2:", data[1])
}

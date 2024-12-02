package main

import (
	"embed"

	"github.com/Toffee1347/adventofcode/utils"
)

//go:embed input/*
var files embed.FS

func getInput(year int, day int, isExample bool) (string, error) {
	filename := utils.IntToStr(year) + "/" + utils.IntToStr(day) + "/"
	if isExample {
		filename += "example"
	} else {
		filename += "input"
	}

	rawData, err := files.ReadFile("input/" + filename)
	if err != nil {
		return "", err
	}
	return string(rawData), nil
}

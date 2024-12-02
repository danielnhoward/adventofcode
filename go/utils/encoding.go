package utils

import (
	"fmt"
	"os"

	"gopkg.in/yaml.v2"
)

func EncodingParseYaml[T any](input string) T {
	var data T

	if err := yaml.Unmarshal([]byte(input), &data); err != nil {
		fmt.Printf("Failed to parse yaml: %v\n", err)
		os.Exit(1)
	}

	return data
}

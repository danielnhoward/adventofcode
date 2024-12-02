package utils

import (
	"strings"
)

func StringCommon(base string, targets ...string) (intersections []rune) {
	for i := 0; i < len(base); i++ {
		character := string(base[i])

		targetsDontContainItem := false
		for _, target := range targets {
			if !strings.Contains(target, character) {
				targetsDontContainItem = true
				break
			}
		}

		if !targetsDontContainItem {
			intersections = append(intersections, rune(base[i]))
		}
	}

	return
}

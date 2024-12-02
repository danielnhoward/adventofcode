package utils

import "regexp"

func GetRegexGroups(regex string, text string) []string {
	operationRegex := regexp.MustCompile(regex)
	results := operationRegex.FindStringSubmatch(text)
	if len(results) > 0 {
		results = results[1:]
	}

	return results
}

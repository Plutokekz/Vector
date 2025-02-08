package main

import (
	"fmt"
	"sort"
	"strings"
)

func PrintGrammarMap(m map[any]map[any]struct{}) {
	// Filter and sort the non-terminal keys
	var keys []string
	for k := range m {
		kStr := fmt.Sprintf("%v", k)
		// Only skip full rules and complex EBNF expressions
		if !strings.Contains(kStr, "=") && // Skip full rules
			!strings.Contains(kStr, ".") { // Skip full rule definitions
			keys = append(keys, kStr)
		}
	}
	sort.Strings(keys)

	if len(keys) == 0 {
		fmt.Println("No keys found in the map")
		return
	}

	// Print each non-terminal with its set
	for _, k := range keys {
		productions := m[k]
		if len(productions) == 0 {
			continue // Skip empty sets
		}

		// Print the non-terminal
		fmt.Printf("\n%s:\n", k)

		// Filter and sort the productions
		var prodList []string
		for prod := range productions {
			prodStr := fmt.Sprintf("%v", prod)
			if prodStr == "" {
				prodStr = "ε" // Use epsilon symbol for empty string
			} else if strings.HasPrefix(prodStr, "\"") && strings.HasSuffix(prodStr, "\"") {
				// Remove quotes from terminals
				prodStr = prodStr[1 : len(prodStr)-1]
			}
			prodList = append(prodList, prodStr)
		}
		sort.Strings(prodList)

		// Print each production
		for _, prod := range prodList {
			fmt.Printf("    → %s\n", prod)
		}
	}
}

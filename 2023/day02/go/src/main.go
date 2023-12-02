package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func part1(input string) int {
	sum := 0

	for line_number, line := range strings.Split(input, "\n") {
		rest := strings.Split(line, ": ")[1]
		words := []string{}
		game_is_possible := true
		word_start := 0

		for i := range rest {
			if rest[i] == ';' || rest[i] == ',' || i == len(rest)-1 {
				if i == len(rest)-1 {
					words = append(words, rest[word_start:i+1])
				} else {
					words = append(words, rest[word_start:i])
				}
				word_start = i + 2
			}
		}

		for _, word := range words {
			value, _ := strconv.Atoi(strings.Split(word, " ")[0])
			colour := strings.Split(word, " ")[1]

			switch colour {
			case "red":
				if value > 12 {
					game_is_possible = false
				}
			case "green":
				if value > 13 {
					game_is_possible = false
				}
			case "blue":
				if value > 14 {
					game_is_possible = false
				}
			}
		}

		if game_is_possible {
			sum += line_number + 1
		}
	}
	return sum
}

func part2(input string) int {
	sum := 0

	for _, line := range strings.Split(input, "\n") {
		rest := strings.Split(line, ": ")[1]
		words := []string{}
		word_start := 0

		max_red := 1
		max_green := 1
		max_blue := 1

		for i := range rest {
			if rest[i] == ';' || rest[i] == ',' || i == len(rest)-1 {
				if i == len(rest)-1 {
					words = append(words, rest[word_start:i+1])
				} else {
					words = append(words, rest[word_start:i])
				}
				word_start = i + 2
			}
		}

		for _, word := range words {
			value, _ := strconv.Atoi(strings.Split(word, " ")[0])
			colour := strings.Split(word, " ")[1]

			switch colour {
			case "red":
				max_red = int(math.Max(float64(max_red), float64(value)))
			case "green":
				max_green = int(math.Max(float64(max_green), float64(value)))
			case "blue":
				max_blue = int(math.Max(float64(max_blue), float64(value)))
			}
		}
		sum += max_red * max_green * max_blue
	}
	return sum

}

func main() {
	data, err := os.ReadFile("../input.txt")
	if err != nil {
		panic(err)
	}
	input := string(data)

	fmt.Println("Part 1:", part1(input))
	fmt.Println("Part 2:", part2(input))
}

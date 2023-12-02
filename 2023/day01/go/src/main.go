package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func convert(input string) int {

	if len(input) == 0 {
		return -1
	}

	// fmt.Print("input: ", input, " ")
	if unicode.IsDigit(rune(input[0])) {
		x, _ := strconv.Atoi(string(input[0]))
		// fmt.Println("digit", x)
		return x
	}

	if strings.HasPrefix(input, "zero") {
		return 0
	} else if strings.HasPrefix(input, "one") {
		return 1
	} else if strings.HasPrefix(input, "two") {
		return 2
	} else if strings.HasPrefix(input, "three") {
		return 3
	} else if strings.HasPrefix(input, "four") {
		return 4
	} else if strings.HasPrefix(input, "five") {
		return 5
	} else if strings.HasPrefix(input, "six") {
		return 6
	} else if strings.HasPrefix(input, "seven") {
		return 7
	} else if strings.HasPrefix(input, "eight") {
		return 8
	} else if strings.HasPrefix(input, "nine") {
		return 9
	} else {
		return -1
	}
}

func part1(input string) int {
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		digits := ""
		for _, c := range line {
			if unicode.IsDigit(c) {
				digits += string(c)
			}
		}
		if len(digits) == 0 {
			continue
		}

		first, err := strconv.Atoi(string(digits[0]))
		if err != nil {
			panic(err)
		}
		last, err := strconv.Atoi(string(digits[len(digits)-1]))
		if err != nil {
			panic(err)
		}
		sum += 10*first + last
	}
	return sum
}

func part2(input string) int {
	sum := 0
	for _, line := range strings.Split(input, "\n") {
		digits := []int{}

		for i := range line {
			value := convert(line[i:])
			if value != -1 {
				digits = append(digits, value)
			}
		}
		sum += 10*digits[0] + digits[len(digits)-1]
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

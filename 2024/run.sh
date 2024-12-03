#!/bin/bash

YEAR=2024

# Check if the user provided a day as an argument
if [ -z "$1" ]; then
    echo "Usage: ./run.sh <day_number>"
    exit 1
fi

# Format the day number to match the folder naming convention
DAY=$(printf "day%02d" "$1")

# Check if the folder exists
if [ ! -d "./$DAY" ]; then
    echo "Error: Folder ./$DAY does not exist."
    exit 1
fi

# Check if main.cpp exists in the specified folder
if [ ! -f "./$DAY/main.cpp" ]; then
    echo "Error: main.cpp does not exist in ./$DAY."
    exit 1
fi

# Compile the main.cpp file
g++ "./$DAY/main.cpp" -o "./$DAY/main" -std=c++17 -Wall -Wextra -Werror -pedantic

# Check if the compilation was successful
if [ $? -ne 0 ]; then
    echo "Error: Compilation failed."
    exit 1
fi

# Run the compiled program
echo "Running ./$DAY/main..."
cd "./$DAY" && ./main

# Clean up the compiled program
rm ./main
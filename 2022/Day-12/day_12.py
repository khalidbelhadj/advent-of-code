from typing import List, Tuple
import numpy as np
import a_star

def parse_grid(text: str) -> Tuple[List[List[int]], Tuple[int, int], Tuple[int, int]]:
    '''Parse input text into a grid'''
    rows = text.split('\n')
    grid = []
    start = tuple()
    end = tuple()
    for i, row in enumerate(rows):
        col = []
        for j, elem in enumerate(row):
            if elem == 'S':
                col.append(float('inf'))
                start = (i, j)
            elif elem == 'E':
                col.append(-1)
                end = (i, j)
            else:
                col.append(122 - ord(elem))
        grid.append(col)
    return (grid, start, end)


def main():
    with open('input.txt', 'r') as file:
        text = file.read()
        grid = parse_grid(text)
        a_star(*grid)
        



if __name__ == '__main__':
    main()


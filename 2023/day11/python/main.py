def manhattan_distance(p1: list[int], p2: list[int]) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def main():
    f = open("../input.txt")
    grid = [l[:-1] for l in f.readlines()]
    f.close()

    galaxies: list[list[int]] = []
    for r in range(len(grid)):
        for c in range(len(grid[r])):
            if grid[r][c] == "#":
                galaxies.append([r, c])

    empty_rows: list[int] = []
    for r in range(len(grid)):
        if all(grid[r][c] == "." for c in range(len(grid[r]))):
            empty_rows.append(r)

    empty_cols: list[int] = []
    for c in range(len(grid[0])):
        if all(grid[r][c] == "." for r in range(len(grid))):
            empty_cols.append(c)

    factor = 1_000_000 - 1
    for r in range(len(galaxies)):
        dr = 0
        for row in empty_rows:
            if row < galaxies[r][0]:
                dr += factor

        dc = 0
        for col in empty_cols:
            if col < galaxies[r][1]:
                dc += factor

        galaxies[r][0] += dr
        galaxies[r][1] += dc

    result = 0
    for r in range(len(galaxies)):
        for c in range(r + 1, len(galaxies)):
            result += manhattan_distance(galaxies[r], galaxies[c])

    print(result)


if __name__ == "__main__":
    main()

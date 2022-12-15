from re import findall

Point = tuple[int, int]


def distance(a: Point, b: Point) -> int:
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def main() -> None:
    beakons: list[Point] = []
    sensors: list[Point] = []
    distances: list[int] = []
    ranges: list[set[int]] = []

    with open('input2.txt', 'r') as file:
        lines = [list(map(int, findall(r'-?\d+', line)))
                 for line in file.readlines()]
        for line in lines:
            [a, b, c, d] = line
            sensors.append((a, b))
            beakons.append((c, d))

    distances = [distance(*pair) for pair in zip(sensors, beakons)]

    # Part 1
    for sensor, dist in zip(sensors, distances):
        dx: int = dist - abs(sensor[1] - 2_000_000)
        if dx <= 0:
            continue
        ranges.append(set(range(sensor[0] - dx, sensor[0] + dx + 1)))
    print(f'Part 1: {len(set.union(*ranges)) - len(set([a for (a, y) in beakons if y == 2_000_000]))}')

    # Part 2
    negative_slopes: list[int] = []
    positive_slopes: list[int] = []
    for (x, y), dist in zip(sensors, distances):
        positive_slopes += [x - y - dist, x - y + dist]
        negative_slopes += [x + y - dist, x + y + dist]

    pos: int = min(list(filter((lambda x: abs(x[0] - x[1]) == 2),
                   [(a, b) for i, a in enumerate(positive_slopes) for b in positive_slopes[i + 1:]]))[0]) + 1
    neg: int = min(list(filter((lambda x: abs(x[0] - x[1]) == 2),
                   [(a, b) for i, a in enumerate(negative_slopes) for b in negative_slopes[i + 1:]]))[0]) + 1

    print(f'Part 2: {(4_000_000 * (pos + neg) // 2) + (neg - pos) // 2}')


if __name__ == '__main__':
    main()

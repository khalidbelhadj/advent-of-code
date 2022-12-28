def follow(head, tail):
    dx, dy = head[0] - tail[0], head[1] - tail[1]
    if abs(dx) < 2 and abs(dy) < 2:
        return
    tail[0] += dx if abs(dx) < 2 else dx - (dx // abs(dx))
    tail[1] += dy if abs(dy) < 2 else dy - (dy // abs(dy))
    return True


def move(head, direction):
    match direction:
        case 'R': head[0] += 1
        case 'L': head[0] -= 1
        case 'U': head[1] += 1
        case 'D': head[1] -= 1


def solve(number_of_knots):
    visited = set()
    knots = [[0, 0] for _ in range(number_of_knots + 1)]

    with open('input.txt', 'r') as file:
        for line in file:
            direction, count = line.split()
            for i in range(int(count)):
                move(knots[0], direction,)
                for j in range(len(knots) - 1):
                    follow(knots[j], knots[j + 1])
                visited.add(tuple(knots[-1]))
    return visited


def main():
    print(f'Part 1: {len(solve(1))}')
    print(f'Part 2: {len(solve(9))}')


if __name__ == '__main__':
    main()

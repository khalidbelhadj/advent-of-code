import re


def part_1():
    with open('input.txt') as file:
        x = [[] for i in range(9)]

        while (line := file.readline()) != '\n':
            for i in range(1, len(line) - 1, 4):
                if line[i].isalpha():
                    x[(i - 1) // 4].append(line[i])

        for line in file:
            ints = [int(s) for s in re.findall(r'\d+', line)]
            
            [moves, frm, to] = ints
            for _ in range(moves):
                x[to - 1][:0] = [x[frm - 1].pop(0)]
        return [y[0] for y in x]

def part_2():
    with open('input.txt') as file:
        x = [[] for i in range(9)]

        while (line := file.readline()) != '\n':
            for i in range(1, len(line) - 1, 4):
                if line[i].isalpha():
                    x[(i - 1) // 4].append(line[i])

        for line in file:
            ints = [int(s) for s in re.findall(r'\d+', line)]

            [moves, frm, to] = ints
            x[to - 1] = x[frm - 1][:moves] + x[to - 1]
            x[frm - 1] = x[frm - 1][moves:]
        return [y[0] for y in x]


def main():
    print(f'Part 1: {part_1()}')
    print(f'Part 2: {part_2()}')

            
if __name__ == '__main__':
    main()

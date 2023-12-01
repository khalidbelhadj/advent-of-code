import re

valves: dict[str, tuple[int, list[str]]] = {}
TOTAL_TIME: int = 30

def parse() -> None:
    global valves

    with open('2022/day_16/example_input.txt', 'r') as file:
        lines = file.readlines()
        for line in lines:
            valve: str = line.split()[1]
            rate: int = int(re.search(r'rate=(\d+);', line).group(1))
            connected: list[str]
            if 'valves' in line:
                connected = line.split(';')[1].removeprefix(' tunnels lead to valves ').split(',')
            else:
                connected = line.split(';')[1].removeprefix(' tunnel leads to valve ').split(',')

            connected = list(map(lambda x: x.replace(' ', ''), connected))
            connected = list(map(lambda x: x.replace('\n', ''), connected))
            valves[valve] = (rate, connected)


def main():
    parse()

if __name__ == '__main__':
    main()

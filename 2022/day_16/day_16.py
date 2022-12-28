import re
from functools import cache

valves: dict[str, tuple[int, list[str]]] = {}
TOTAL_TIME: int = 30


@cache
def max_score(pos: str, time: int, opened: frozenset[str], with_elephant: bool = False) -> int:
    if time == 0:
        if with_elephant:
            return max_score('AA', TOTAL_TIME - 4, opened)
        return 0
    time -= 1
    score = max([max_score(new_pos, time, opened, with_elephant) for new_pos in valves[pos][1]])
    if valves[pos][0] > 0 and pos not in opened:
        new_opened: set[str] = set(opened)
        new_opened.add(pos)
        score = max(
            score,
            time * valves[pos][0] + max_score(pos, time, frozenset(new_opened), with_elephant)
        )
    return score


def parse() -> None:
    global valves

    with open('input.txt', 'r') as file:
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


def main() -> None:
    parse()
    print(f'Part 1: {max_score("AA", TOTAL_TIME, frozenset())}')
    print(f'Part 2: {max_score("AA", TOTAL_TIME - 4, frozenset(), True)}')



if __name__ == '__main__':
    main()
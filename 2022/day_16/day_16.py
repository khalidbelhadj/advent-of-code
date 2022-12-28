import re
from heapq import heappop, heappush

valves: dict = {}
distances: dict[str, dict] = {}
valves_with_pressure: list = []
TOTAL_TIME_1: int = 30

def max_flow():
    queue = [(0, 0, "AA", ("AA",))]
    highest_pressure_per_path: dict = {}

    while queue:
        pressure_released, time_elapsed, current_valve, current_path = heappop(queue)

        for val, dist in distances[current_valve].items():


            # 1 min for each unit dist +1 for valve opening time8
            next_time_elapsed = time_elapsed + dist + 1

            if val in current_path or next_time_elapsed > TOTAL_TIME_1: continue
            
            # Adding the total remaining time times the pressure released per minute from val
            next_pressure_released = pressure_released + (TOTAL_TIME_1 - next_time_elapsed) * valves[val][0]


            next_path = tuple(list(current_path) + [val])

            if (next_path not in highest_pressure_per_path or 
                highest_pressure_per_path[next_path] < next_pressure_released):
                
                highest_pressure_per_path[next_path] = next_pressure_released
                heappush(queue, (next_pressure_released, next_time_elapsed, val, next_path))

    return max(highest_pressure_per_path.values())


def get_distances() -> None:
    global distances

    valves_with_pressure.append('AA')
    for start_valve in valves_with_pressure:

        distances_from_valve: dict = {}
        queue: list = [(0, start_valve)]
        visited: set = set(start_valve)
        
        while queue:
            dist, valve = heappop(queue)
            for v in valves[valve][1]: # v in list of connected valves

                if v in visited: continue

                visited.add(v)

                if v in valves_with_pressure and v != start_valve:
                    distances_from_valve[v] = dist + 1

                heappush(queue, (dist + 1, v))
        distances[start_valve] = dict(distances_from_valve)


def parse() -> None:
    global valves, valves_with_pressure

    with open('input.txt', 'r') as file:
        lines = file.readlines()
        for line in lines:
            valve: str = line.split()[1]
            rate: int = int(re.search('rate=(\d+);', line).group(1))
            if 'valves' in line:
                connected: list[str] = line.split(';')[1].removeprefix(' tunnels lead to valves ').split(',')
            else:
                connected: list[str] = line.split(';')[1].removeprefix(' tunnel leads to valve ').split(',')

            connected = list(map(lambda x: x.replace(' ', ''), connected))
            connected = list(map(lambda x: x.replace('\n', ''), connected))
            valves[valve] = (rate, connected)

    valves_with_pressure = [v for v in valves if valves[v][0] > 0]


def main() -> None:
    parse()
    get_distances()

    print(f'Part 1: {max_flow()}')
    


if __name__ == '__main__':
    main()
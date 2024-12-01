from collections import deque
import enum

# Pipe connectivity mapping
DIRECTIONS: dict[str, list[tuple[int, int]]] = {
    "|": [(0, -1), (0, 1)],  # Vertical pipe: north-south
    "-": [(-1, 0), (1, 0)],  # Horizontal pipe: east-west
    "L": [(0, -1), (1, 0)],  # Bend: north-east
    "J": [(0, -1), (-1, 0)],  # Bend: north-west
    "7": [(0, 1), (-1, 0)],  # Bend: south-west
    "F": [(0, 1), (1, 0)],  # Bend: south-east
    "S": [],  # Start point (shape to be determined)
}

# Reverse the direction
REVERSE = {(-1, 0): (1, 0), (1, 0): (-1, 0), (0, -1): (0, 1), (0, 1): (0, -1)}


def parse_input(file: str):
    """Parse the grid from the input file."""
    with open(file, "r") as f:
        grid = [list(line.strip()) for line in f]
    return grid


def find_start(grid: list[list[str]]):
    """Find the position of the starting point S."""
    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == "S":
                return x, y
    return None


def neighbors(x: int, y: int, grid: list[list[str]], pipe_type: str):
    """Get valid neighbors for a given pipe."""
    valid_neighbors: list[tuple[int, int]] = []
    for dx, dy in DIRECTIONS[pipe_type]:
        nx, ny = x + dx, y + dy
        if 0 <= ny < len(grid) and 0 <= nx < len(grid[ny]):
            neighbor_type = grid[ny][nx]
            if neighbor_type != "." and REVERSE[(dx, dy)] in DIRECTIONS[neighbor_type]:
                valid_neighbors.append((nx, ny))
    return valid_neighbors


def bfs_find_farthest(grid: list[list[str]], start: tuple[int, int]):
    """Use BFS to find the farthest point from the start."""
    queue = deque([(start, 0)])
    visited = set([start])
    max_distance = 0

    while queue:
        (x, y), distance = queue.popleft()
        max_distance = max(max_distance, distance)
        pipe_type = grid[y][x]

        # Determine neighbors based on pipe type
        for nx, ny in neighbors(x, y, grid, pipe_type):
            if (nx, ny) not in visited:
                visited.add((nx, ny))
                queue.append(((nx, ny), distance + 1))

    return (max_distance, visited)


def solve(file: str):
    """Solve the problem and print the result."""
    grid = parse_input(file)
    start = find_start(grid)
    if not start:
        print("No starting position 'S' found in the grid.")
        return

    # Treat S as connected initially in all possible directions
    DIRECTIONS["S"] = DIRECTIONS["F"]  # Assuming S is a bend like F
    dist, visited = bfs_find_farthest(grid, start)
    print(f"Part 1: {dist}")

    interior_count = 0
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if (x, y) in visited:
                continue

            right = 0
            for nx in range(x + 1, len(grid[y])):
                if (nx, y) in visited and grid[y][nx] in ["|", "L", "J"]:
                    right += 1

            if right % 2 == 1:
                interior_count += 1

    print(f"Part 2: {interior_count}")


# Usage
# Save the input grid in a file (e.g., "input.txt") and provide the file path below
solve("../input.txt")

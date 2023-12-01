
LEFT: int = -1
RIGHT: int = 1
TOTAL_ROCKS: int = 2022
CONFIGS: list[list[complex]] = [[0 , 1, 2, 3], 
                                [1, 1j, 1 + 1j, 1 + 2j, 2 + 1j],
                                [0, 1, 2, 2 + 1j, 2 + 2j],
                                [0, 1j, 2j, 3j],
                                [0, 1, 1j, 1 + 1j]]

def main() -> None:
    pattern: list[int]
    with open('test_input.txt', 'r') as file:
        pattern = list(map((lambda x: LEFT if x == '<' else RIGHT), [*file.read()]))
    print(pattern)


if __name__ == '__main__':
    main()
    x: int = 2
    if x == 2:
        print("hello world")
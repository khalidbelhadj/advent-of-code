
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
    
    with open('2022/Day-17/input2.txt', 'r') as file:
        pattern = list(map((lambda x: LEFT if x == '<' else RIGHT), [*file.read()]))

    
    

if __name__ == '__main__':
    main()
def find_signal(text: str, marker_length: int) -> int:
    for i in range(len(text) - marker_length):
        s = text[i:i + marker_length]
        if (len(s) == len(list(set(s)))):
            return i + marker_length
    return -1


def main() -> None:
    with open('input.txt') as file:
        text = file.read()
        print(f'Part 1: {find_signal(text, 4)}')
        print(f'Part 2: {find_signal(text, 14)}')

if __name__ == '__main__':
    main()

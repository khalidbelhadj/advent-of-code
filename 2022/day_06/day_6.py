def find_signal(file, marker_length):
    for i in range(len(file) - marker_length):
        s = file[i:i + marker_length]
        if (len(s) == len(list(set(s)))):
            return i + marker_length


def main():
    file = open('input.txt')
    file = file.read()
    print(type(file))
    print(f'Part 1: {find_signal(file, 4)}')
    print(f'Part 2: {find_signal(file, 14)}')

if __name__ == '__main__':
    main()

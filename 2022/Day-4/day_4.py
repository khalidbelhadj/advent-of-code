def parse(s):
    start = int(s[:(s.index('-'))])
    end = int(s[(s.index('-') + 1):])
    return set(range(start, end + 1))


def main():
    with open('input.txt') as file:
        full_overlap = 0
        some_overlap = 0
        for line in file:
            [sections1, sections2] = map(parse, line[:-1].split(','))
            full_overlap += (sections1 <= sections2 or sections2 <= sections1)
            some_overlap += (len(sections1.intersection(sections2)) != 0)

        print(f'Part 1: {full_overlap}')
        print(f'Part 2: {some_overlap}')


if __name__ == '__main__':
    main()

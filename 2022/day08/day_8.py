import numpy as np

def max_index(xs, n):
    max = 0
    for i, elem in enumerate(xs):
        max = i + 1
        if elem >= n:
            break
    return max


def main():
    with open('input.txt', 'r') as file:
        input = list(map((lambda x : list(map((lambda y : int(y)) ,[*x[:-1]]))) ,file.readlines()))
        input_T = np.array(input).T.tolist()
        inside_total = (len(input) - 2) * (len(input[0]) - 2)
        outside = 2 * (len(input)) + 2 * (len(input[0]) - 2)
        max_scenic = 0
        invisible = 0

        for i in range(1, len(input) - 1):
            for j in range(1, len(input[i]) - 1):
                curr_tree = input[i][j]

                left = input[i][0:j]
                right = input[i][j + 1:]
                up = input_T[j][0:i]
                down = input_T[j][i + 1:]

                invisible += (max(left) >= curr_tree and
                              max(right) >= curr_tree and
                              max(up) >= curr_tree and
                              max(down) >= curr_tree)

                scenic_score = (max_index(reversed(left), curr_tree) *
                                max_index(right, curr_tree) *
                                max_index(reversed(up), curr_tree) *
                                max_index(down, curr_tree))

                max_scenic = scenic_score if scenic_score > max_scenic else max_scenic
                
        print(f'Part 1: {inside_total - invisible + outside}')
        print(f'Part 2: {max_scenic}')
                
if __name__ == '__main__':
    main()

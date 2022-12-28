class File:
    def __init__(self, size, name, parent):
        self.size = size
        self.name = name
        self.parent = parent


class Dir:
    def __init__(self, size, name, parent, childs):
        self.size = size
        self.name = name
        self.childs = childs
        self.parent = parent


def main():
    home = Dir(size=0, name='/', parent=None, childs=[])
    pointer = home
    ls = False  # True when next line to be read is an ls output
    directories = []
    with open('input.txt', 'r') as file:
        file.readline()  # Getting rid of first line '$ cd /'
        for line in file:
            if '$ cd' in line:
                ls = False
                (_, _, target) = line.split()
                if target == '..':
                    pointer = pointer.parent
                    continue
                for child in pointer.childs:
                    if child.name == target:
                        pointer = child
                        break
            if ls:
                (dir_or_size, name) = line.split()
                if dir_or_size == 'dir':
                    dr = Dir(size=0, name=name, parent=pointer, childs=[])
                    pointer.childs.append(dr)
                    directories.append(dr)
                else:
                    fl = File(size=int(dir_or_size), name=name, parent=pointer)
                    pointer.childs.append(fl)
                    ptr = fl.parent
                    while ptr is not None:
                        ptr.size += int(dir_or_size)
                        ptr = ptr.parent
            elif '$ ls' in line:
                ls = True
    required_space = 30000000 - 70000000 - home.size
    print(
        f'Part 1: {sum(filter((lambda s : s <= 100000) ,map((lambda x : x.size), directories)))}')
    print(
        f'Part 2: {min(filter((lambda s : s >= required_space) ,map((lambda x : x.size), directories)))}')


if __name__ == '__main__':
    main()

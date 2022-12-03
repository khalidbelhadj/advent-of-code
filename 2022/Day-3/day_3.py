
def priority(ch):
    return ord(ch) - 38 if ch.isupper() else ord(ch) - 96

def main():
    with open('input.txt', 'r') as file:
        shared_items = []
        
        backpacks = []
        badges = []
        counter = 0
        for line in file:
            
            counter = (counter + 1) % 3
            backpacks.append(line[:-1])
           
            if (counter == 0 and backpacks != 0):
                badge = set.intersection(*[set(b) for b in backpacks])
                print(badge)
                badges.append(priority(list(badge)[0]))
                backpacks = []
                
            n = len(line) // 2
            shared_item = set(line[:n]).intersection(set(line[n:]))
            shared_items.append(list(shared_item)[0])
            
        print(f'Part 1: {sum(list(map(priority, shared_items)))}')
        print(f'Part 2: {sum(badges)}')


if __name__ == '__main__':
    main()

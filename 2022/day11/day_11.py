from typing import Callable
from dataclasses import dataclass
from re import search, findall
from math import prod
from numpy import lcm

reduction_constant: float = float('inf')


@dataclass
class Monkey:
    items: list[int]
    operation: Callable
    test: int
    true_monkey: int
    false_monkey: int
    inspections: int = 0

    def inspect(self) -> tuple[int, int]:
        item: int = self.items.pop(0)
        item = self.operation(item)
        self.inspections += 1
        return (self.true_monkey, item) if item % self.test == 0 else (self.false_monkey, item)

    def receive(self, item) -> None:
        self.items.append(item % reduction_constant)


def throw(from_monkey: Monkey, monkeys: list[Monkey]) -> None:
    (to_monkey, item) = from_monkey.inspect()
    to_monkey = monkeys[to_monkey]
    to_monkey.receive(item)


def parse_monkey(text: str) -> Monkey | None:
    components: list[str] = text.split('\n  ')
    try:
        items: list[int] = list(map(int, findall(r'\d+', components[1])))
        operation: Callable = parse_operation(components[2])
        test: int = int(search('Test: divisible by (\d+)', components[3]).group(1))
        true_monkey: int = int(search('  If true: throw to monkey (\d+)', components[4]).group(1))
        false_monkey: int = int(search('  If false: throw to monkey (\d+)', components[5]).group(1))
        return Monkey(items, operation, test, true_monkey, false_monkey)
    except:
        print('Error parsing monkey')
        return None


def parse_monkeys() -> list[Monkey]:
    monkey_list: list[Monkey] = []
    with open('input.txt') as file:
        monkeys_raw: list[str] = file.read().split('\n\n')
        for monkey in monkeys_raw:
            monkey_list.append(parse_monkey(monkey))
    return monkey_list


def parse_operation(text: str) -> Callable:
    operator: str = search('Operation: new = old (.*) (.*)', text).group(1)
    operand: str = search('Operation: new = old (.*) (.*)', text).group(2)

    if operand == 'old':
        return (lambda x: x * x)
    if operator == '+':
        return (lambda x: x + int(operand))
    if operator == '*':
        return (lambda x: x * int(operand))


def solve(monkey_list: list[Monkey], rounds) -> int:
    for _ in range(rounds):
        for monkey in monkey_list:
            while monkey.items != []:
                throw(monkey, monkey_list)
    return (prod(sorted([monkey.inspections for monkey in monkey_list])[-2:]))


def main():
    monkey_list_1: list[Monkey] = parse_monkeys()
    monkey_list_2: list[Monkey] = parse_monkeys()

    print(f'Part 1: {solve(monkey_list_1, 20)}')

    global reduction_constant
    reduction_constant = lcm.reduce([monkey.test for monkey in monkey_list_2])

    print(f'Part 2: {solve(monkey_list_2, 10000)}')


if __name__ == '__main__':
    main()

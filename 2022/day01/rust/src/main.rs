use std::fs;

fn main() {
    let content =
        fs::read_to_string("../input.txt").expect("Something went wrong reading the file");
    let mut elves: Vec<i32> = vec![];
    let lines = content.lines();

    let mut tmp = vec![];
    for line in lines {
        if line.is_empty() {
            elves.push(
                tmp.into_iter()
                    .map(|x: String| x.parse::<i32>().expect("could not parse"))
                    .sum(),
            );
            tmp = vec![];
        } else {
            tmp.push(line.to_string());
        }
    }
    elves.push(
        tmp.into_iter()
            .map(|x: String| x.parse::<i32>().expect("could not parse"))
            .sum(),
    );
    elves.sort();

    println!("Part 1: {}", elves.last().unwrap());
    println!("Part 2: {}", elves.iter().rev().take(3).sum::<i32>());
}

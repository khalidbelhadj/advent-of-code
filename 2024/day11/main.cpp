#include <cmath>
#include <iostream>
#include <list>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "../utils.cpp"

using namespace std;

map<pair<long, long>, long> cache;

long calculate(long n, long steps) {
    if (cache.find({n, steps}) != cache.end()) return cache[{n, steps}];
    if (steps == 0) return 1;
    if (n == 0) {
        long result = calculate(1, steps - 1);
        cache[{n, steps}] = result;
        return result;
    }

    string s = to_string(n);
    if (s.size() % 2 == 0) {
        int stone1 = stol(s.substr(0, s.size() / 2));
        int stone2 = stol(s.substr(s.size() / 2));
        long result =
            calculate(stone1, steps - 1) + calculate(stone2, steps - 1);
        cache[{n, steps}] = result;
        return result;
    }

    long result = calculate(n * 2024, steps - 1);
    cache[{n, steps}] = result;
    return result;
}

int main() {
    string input = read_to_string("./input.txt");

    vector<long> stones;
    size_t pos = 0;
    while ((pos = input.find(" ")) != string::npos) {
        stones.push_back(stol(input.substr(0, pos)));
        input.erase(0, pos + 1);
    }
    stones.push_back(stol(input));

    long part_1 = 0;
    for (auto stone : stones) {
        part_1 += calculate(stone, 25);
    }

    long part_2 = 0;
    for (auto stone : stones) {
        part_2 += calculate(stone, 75);
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
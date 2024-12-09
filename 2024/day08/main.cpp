#include <iostream>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include "../utils.cpp"

using namespace std;

int find_gcd(int a, int b) {
    if (b == 0) return a;
    return find_gcd(b, a % b);
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    unordered_map<char, vector<pair<int, int>>> antennas;

    for (size_t i = 0; i < lines.size(); ++i) {
        for (size_t j = 0; j < lines[i].size(); ++j) {
            if (lines[i][j] == '.') continue;
            antennas[lines[i][j]].push_back({i, j});
        }
    }

    set<pair<int, int>> antinodes;

    int part_1 = 0;
    int part_2 = 0;

    auto in_bound = [&lines](pair<int, int> p) {
        return p.first >= 0 && p.first < (int)lines.size() && p.second >= 0 &&
               p.second < (int)lines[0].size();
    };

    // Part 1
    for (const auto& [antenna, positions] : antennas) {
        for (size_t i = 0; i < positions.size(); ++i) {
            for (size_t j = 0; j < positions.size(); ++j) {
                if (i == j) continue;

                auto [ay, ax] = positions[i];
                auto [by, bx] = positions[j];

                auto dy = ay - by;
                auto dx = ax - bx;

                pair<int, int> antinode = {ay + dy, ax + dx};

                if (in_bound(antinode)) {
                    antinodes.insert(antinode);
                }
            }
        }
    }
    part_1 = antinodes.size();

    antinodes.clear();

    // Part 2
    for (const auto& [antenna, positions] : antennas) {
        for (size_t i = 0; i < positions.size(); ++i) {
            for (size_t j = 0; j < positions.size(); ++j) {
                if (i == j) continue;

                auto [ay, ax] = positions[i];
                auto [by, bx] = positions[j];

                antinodes.insert({ay, ax});

                int dy = ay - by;
                int dx = ax - bx;

                int gcd = find_gcd(abs(dy), abs(dx));
                dy /= gcd;
                dx /= gcd;

                pair<int, int> antinode = {ay + dy, ax + dx};
                while (in_bound(antinode)) {
                    antinodes.insert(antinode);
                    antinode.first += dy;
                    antinode.second += dx;
                }
            }
        }
    }

    part_2 = antinodes.size();

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
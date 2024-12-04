#include <cstddef>
#include <iostream>
#include <set>
#include <string>
#include <utility>

#include "../utils.cpp"

using namespace std;

bool match(vector<string> lines, size_t r, size_t c, pair<int, int> dir,
           string target) {
    size_t n = lines.size();
    size_t m = lines[0].size();

    for (size_t i = 0; i < target.size(); ++i) {
        size_t new_r = r + i * dir.first;
        size_t new_c = c + i * dir.second;

        if (new_r < 0 || new_r >= n || new_c < 0 || new_c >= m) {
            return false;
        }

        if (lines[new_r][new_c] != target[i]) {
            return false;
        }
    }

    return true;
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");
    size_t n = lines.size();
    size_t m = lines[0].size();

    size_t part_1 = 0;
    size_t part_2 = 0;
    set<pair<size_t, size_t>> xs;

    for (size_t r = 0; r < n; ++r) {
        for (size_t c = 0; c < m; ++c) {
            pair<int, int> dir[] = {{0, 1}, {0, -1}, {1, 0},  {-1, 0},
                                    {1, 1}, {1, -1}, {-1, 1}, {-1, -1}};
            pair<int, int> diag[] = {{1, 1}, {1, -1}, {-1, 1}, {-1, -1}};

            for (auto d : dir) {
                part_1 += match(lines, r, c, d, "XMAS");
            }

            for (auto d : diag) {
                if (match(lines, r, c, d, "MAS")) {
                    pair<size_t, size_t> A_pos = {r + d.first, c + d.second};

                    if (xs.find(A_pos) != xs.end()) {
                        xs.erase(A_pos);
                        part_2 += 1;
                    } else {
                        xs.insert(A_pos);
                    }
                }
            }
        }
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
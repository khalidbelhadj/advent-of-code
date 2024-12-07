#include <iostream>
#include <set>
#include <string>
#include <utility>

#include "../utils.cpp"

using namespace std;

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    int start_r = 0;
    int start_c = 0;
    for (size_t i = 0; i < lines.size(); i++) {
        for (size_t j = 0; j < lines[i].size(); j++) {
            if (lines[i][j] == '^') {
                start_r = i;
                start_c = j;
            }
        }
    }

    int r = start_r;
    int c = start_c;

    pair<int, int> d = {-1, 0};
    set<pair<int, int>> visited;
    visited.insert({r, c});

    while (true) {
        int new_r = r + d.first;
        int new_c = c + d.second;

        if (new_r < 0 || new_r >= (int)lines.size() || new_c < 0 ||
            new_c >= (int)lines[0].size()) {
            break;
        }

        if (lines[new_r][new_c] == '#') {
            if (d.first == -1) {
                d = {0, 1};
            } else if (d.first == 1) {
                d = {0, -1};
            } else if (d.second == -1) {
                d = {-1, 0};
            } else if (d.second == 1) {
                d = {1, 0};
            }
        } else {
            r = new_r;
            c = new_c;
            visited.insert({r, c});
        }
    }

    int part_1 = visited.size();
    int part_2 = 0;

    for (size_t i = 0; i < lines.size(); i++) {
        for (size_t j = 0; j < lines[i].size(); j++) {
            if (lines[i][j] != '.') continue;
            cout << i << " " << j << endl;

            lines[i][j] = '#';

            int r = start_r;
            int c = start_c;

            pair<int, int> d = {-1, 0};
            set<pair<pair<int, int>, pair<int, int>>> visited;
            visited.insert({{r, c}, d});

            while (true) {
                int new_r = r + d.first;
                int new_c = c + d.second;

                if (new_r < 0 || new_r >= (int)lines.size() || new_c < 0 ||
                    new_c >= (int)lines[0].size()) {
                    break;
                }

                if (lines[new_r][new_c] == '#') {
                    if (d.first == -1) {
                        d = {0, 1};
                    } else if (d.first == 1) {
                        d = {0, -1};
                    } else if (d.second == -1) {
                        d = {-1, 0};
                    } else if (d.second == 1) {
                        d = {1, 0};
                    }
                } else {
                    r = new_r;
                    c = new_c;
                    pair<pair<int, int>, pair<int, int>> entry = {{r, c}, d};
                    if (visited.count(entry)) {
                        part_2++;
                        break;
                    }
                    visited.insert(entry);
                }
            }

            lines[i][j] = '.';
        }
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
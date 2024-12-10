#include <iostream>
#include <queue>
#include <set>
#include <string>
#include <utility>

#include "../utils.cpp"

using namespace std;

pair<long, long> bfs(const vector<string> &lines, int sr, int sc) {
    queue<pair<int, int>> q;
    q.push({sr, sc});

    int layer = 1;
    while (!q.empty() && layer < 10) {
        int layer_size = q.size();
        for (int i = 0; i < layer_size; ++i) {
            auto [r, c] = q.front();
            q.pop();

            int dr[] = {0, 0, 1, -1};
            int dc[] = {1, -1, 0, 0};

            for (int j = 0; j < 4; ++j) {
                int nr = r + dr[j];
                int nc = c + dc[j];

                if (nr < 0 || nr >= (int)lines.size() || nc < 0 ||
                    nc >= (int)lines[0].size()) {
                    continue;
                }

                if (lines[nr][nc] - '0' != layer) {
                    continue;
                }

                q.push({nr, nc});
            }
        }
        ++layer;
    }

    long path_count = q.size();

    set<pair<int, int>> dest;
    while (!q.empty()) {
        auto [r, c] = q.front();
        q.pop();
        dest.insert({r, c});
    }

    return {dest.size(), path_count};
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    long part_1 = 0;
    long part_2 = 0;
    for (size_t i = 0; i < lines.size(); ++i) {
        for (size_t j = 0; j < lines[i].size(); ++j) {
            if (lines[i][j] == '0') {
                auto [a, b] = bfs(lines, i, j);
                part_1 += a;
                part_2 += b;
            }
        }
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
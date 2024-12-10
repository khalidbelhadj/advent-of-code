#include <iostream>
#include <list>
#include <string>
#include <unordered_map>
#include <vector>

#include "../utils.cpp"

using namespace std;

long part_1(string input) {
    int id = 0;
    bool is_file = true;
    vector<int> v;
    for (char c : input) {
        int x = c - '0';
        if (is_file) {
            for (int i = 0; i < x; i++) v.push_back(id);
            id += 1;
        } else {
            for (int i = 0; i < x; i++) v.push_back(-1);
        }
        is_file = !is_file;
    }

    int first_free = 0;
    int last = v.size() - 1;

    while (v[first_free] != -1) first_free += 1;
    while (v[last] == -1) last -= 1;

    while (first_free < last) {
        v[first_free] = v[last];
        v[last] = -1;
        while (v[first_free] != -1) first_free += 1;
        while (v[last] == -1) last -= 1;
    }

    long res = 0;
    for (size_t i = 0; i < v.size(); i++) {
        if (v[i] == -1) break;
        res += v[i] * i;
    }
    return res;
}

long part_2(string input) {
    // {start, length}
    unordered_map<int, pair<int, int>> files;
    vector<pair<int, int>> blanks;

    int id = 0;
    bool is_file = true;
    int pos = 0;
    vector<pair<int, int>> v;
    for (char c : input) {
        int x = c - '0';
        if (is_file) {
            if (x == 0) exit(1);
            files[id] = {pos, x};
            id++;
        } else if (x != 0) {
            blanks.push_back({pos, x});
        }
        is_file = !is_file;
        pos += x;
    }

    while (id > 0) {
        id -= 1;
        auto [f_pos, f_len] = files[id];
        for (size_t i = 0; i < blanks.size(); i++) {
            auto [b_pos, b_len] = blanks[i];
            if (b_pos >= f_pos) {
                break;
            }

            if (f_len <= b_len) {
                files[id] = {b_pos, f_len};
                if (b_len == f_len) {
                    blanks.erase(blanks.begin() + i);
                } else {
                    blanks[i] = {b_pos + f_len, b_len - f_len};
                }
                break;
            }
        }
    }

    long res = 0;

    for (auto [id, val] : files) {
        auto [pos, len] = val;
        for (int i = pos; i < pos + len; ++i) {
            res += i * id;
        }
    }

    return res;
}

int main() {
    string input = read_to_string("./input.txt");

    cout << "Part 1: " << part_1(input) << endl;
    cout << "Part 2: " << part_2(input) << endl;
    return 0;
}
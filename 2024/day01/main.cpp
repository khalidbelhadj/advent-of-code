#include <algorithm>
#include <cstddef>
#include <iostream>
#include <string>
#include <unordered_map>

#include "../utils.cpp"

using namespace std;

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    vector<int> left;
    vector<int> right;

    for (string line : lines) {
        vector<string> v;
        string s;
        stringstream ss(line);
        while (getline(ss, s, ' ')) {
            if (s.empty()) continue;
            v.push_back(s);
        }

        left.push_back(stoi(v[0]));
        right.push_back(stoi(v[1]));
    }

    sort(left.begin(), left.end());
    sort(right.begin(), right.end());

    size_t part_1 = 0;
    for (size_t i = 0; i < left.size(); i++) {
        part_1 += abs(left[i] - right[i]);
    }

    unordered_map<int, int> right_count;

    for (size_t i = 0; i < right.size(); i++) {
        right_count[right[i]]++;
    }

    int part_2 = 0;

    for (int num : left) {
        int count = right_count[num];
        part_2 += count * num;
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
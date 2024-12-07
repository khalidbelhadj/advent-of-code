#include <cmath>
#include <cstdio>
#include <iostream>
#include <string>

#include "../utils.cpp"

using namespace std;

bool is_valid(size_t idx, const vector<long>& values, long target,
              long current) {
    if (idx == values.size()) return current == target;

    long times = values[idx] * current;
    long plus = values[idx] + current;

    return is_valid(idx + 1, values, target, times) ||
           is_valid(idx + 1, values, target, plus);
}

bool is_valid_concat(size_t idx, const vector<long>& values, long target,
                     long current) {
    if (idx == values.size()) return current == target;

    long times = values[idx] * current;
    long plus = values[idx] + current;
    int digits = floor(log10(values[idx]) + 1);
    long concat = (pow(10, digits)) * current + values[idx];

    return is_valid_concat(idx + 1, values, target, times) ||
           is_valid_concat(idx + 1, values, target, plus) ||
           is_valid_concat(idx + 1, values, target, concat);
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    long part_1 = 0;
    long part_2 = 0;

    for (string line : lines) {
        auto colon = line.find(":");
        long target = stol(line.substr(0, colon));

        string rest = line.substr(colon + 2);
        vector<long> values;
        string s;
        stringstream ss(rest);
        while (getline(ss, s, ' ')) values.push_back(stoi(s));

        if (is_valid(1, values, target, values[0])) {
            part_1 += target;
        }

        if (is_valid_concat(1, values, target, values[0])) {
            part_2 += target;
        }
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
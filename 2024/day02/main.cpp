#include <iostream>
#include <string>

#include "../utils.cpp"

using namespace std;

bool check(const vector<int> v) {
    int sign = (v[1] - v[0]) / abs(v[1] - v[0]);
    for (size_t i = 1; i < v.size(); ++i) {
        int delta = v[i] - v[i - 1];
        if (sign != delta / abs(delta)) return false;
        if (abs(delta) == 0 || abs(delta) > 3) return false;
    }
    return true;
}

bool check_remove(const vector<int>& v) {
    for (size_t i = 0; i < v.size(); ++i) {
        vector<int> modified = v;
        modified.erase(modified.begin() + i);
        if (check(modified)) {
            return true;
        }
    }
    return false;
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    int part_1 = 0;
    int part_2 = 0;
    for (auto& line : lines) {
        vector<int> v;
        string s;
        stringstream ss(line);
        while (getline(ss, s, ' ')) {
            v.push_back(stoi(s));
        }
        part_1 += check(v);
        part_2 += check_remove(v);
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
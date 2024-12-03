#include <cstddef>
#include <iostream>
#include <regex>
#include <string>

#include "../utils.cpp"

using namespace std;

int main() {
    string input = read_to_string("./input.txt");

    sregex_iterator begin;
    sregex_iterator end;

    vector<pair<int, bool>> conds;

    regex do_pattern(R"(do\(\))");
    begin = sregex_iterator(input.begin(), input.end(), do_pattern);
    end = sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        smatch match = *it;
        int index = match.position();
        conds.push_back({index, true});
    }

    regex dont_pattern(R"(don\'t\(\))");
    begin = sregex_iterator(input.begin(), input.end(), dont_pattern);
    end = sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        smatch match = *it;
        int index = match.position();
        conds.push_back({index, false});
    }

    sort(conds.begin(), conds.end());

    size_t part_1 = 0;
    size_t part_2 = 0;
    size_t i = 0;
    bool enabled = true;

    regex pattern(R"(mul\((\d{1,3}),(\d{1,3})\))");
    begin = sregex_iterator(input.begin(), input.end(), pattern);
    end = sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        smatch match = *it;
        int a = stoi(match.str(1));
        int b = stoi(match.str(2));
        part_1 += a * b;

        int index = match.position();
        while (i < conds.size() && conds[i].first < index) {
            enabled = conds[i].second;
            i++;
        }

        if (enabled) part_2 += a * b;
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
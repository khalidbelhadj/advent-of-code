#include <cmath>
#include <cstddef>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "../utils.cpp"

using namespace std;

vector<size_t> get_values(string query) {
    vector<size_t> values;
    size_t pos = 0;
    while ((pos = query.find(",")) != string::npos) {
        values.push_back(stoi(query.substr(0, pos)));
        query.erase(0, pos + 1);
    }
    values.push_back(stoi(query));
    return values;
}

int main() {
    vector<string> lines = read_to_lines("./input.txt");

    // Parsing rules and queries
    vector<string> rules;
    vector<string> queries;

    bool rules_done = false;
    for (string line : lines) {
        if (line.empty()) {
            rules_done = true;
            continue;
        }
        if (rules_done) {
            queries.push_back(line);
        } else {
            rules.push_back(line);
        }
    }

    // Building graph
    unordered_map<size_t, unordered_set<size_t>> back_graph;
    unordered_map<size_t, unordered_set<size_t>> front_graph;
    size_t part_1 = 0;
    size_t part_2 = 0;

    for (string rule : rules) {
        if (rule == "") break;
        size_t a = stoi(rule.substr(0, rule.find("|")));
        size_t b = stoi(rule.substr(rule.find("|") + 1));
        back_graph[b].insert(a);
        front_graph[a].insert(b);
    }

    // Processing queries
    for (string query : queries) {
        vector<size_t> values = get_values(query);

        bool ok = true;
        for (size_t i = 0; i < values.size(); i++) {
            for (size_t j = i + 1; j < values.size(); j++) {
                size_t val1 = values[i];
                size_t val2 = values[j];

                if (back_graph[val1].find(val2) != back_graph[val1].end()) {
                    ok = false;
                    break;
                }
            }
        }

        if (ok) {
            part_1 += values[floor(values.size() / 2)];
        } else {
            // Topological sort
            vector<size_t> sorted;
            queue<size_t> Q;
            unordered_map<size_t, size_t> D;
            unordered_set<size_t> value_set(values.begin(), values.end());

            for (size_t v : values) {
                for (size_t u : back_graph[v]) {
                    if (value_set.find(u) != value_set.end()) {
                        D[v]++;
                    }
                }
            }

            for (size_t v : values) {
                if (D[v] == 0) Q.push(v);
            }

            while (!Q.empty()) {
                size_t x = Q.front();
                Q.pop();
                // cout << x << endl;
                sorted.push_back(x);
                for (size_t u : front_graph[x]) {
                    if (D.find(u) != D.end()) {
                        D[u]--;
                        if (D[u] == 0) {
                            Q.push(u);
                        }
                    }
                }
            }

            part_2 += sorted[floor(sorted.size() / 2)];
        }
    }

    cout << "Part 1: " << part_1 << endl;
    cout << "Part 2: " << part_2 << endl;
    return 0;
}
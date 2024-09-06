#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

int part_1(string input) {
  string line;
  auto sum = 0;
  stringstream ss(input);

  while (getline(ss, line, '\n')) {
    string digits;
    copy_if(line.begin(), line.end(), back_inserter(digits),
            [](char c) { return std::isdigit(c); });

    string number;

    if (digits.empty()) continue;

    number.push_back(digits.front());
    number.push_back(digits.back());
    sum += stoi(number);
  }
  return sum;
}

int convert(string input) {
  static const std::unordered_map<std::string, int> digitMap = {
      {"zero", 0}, {"one", 1}, {"two", 2},   {"three", 3}, {"four", 4},
      {"five", 5}, {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9}};

  if (input.empty()) return -1;
  if (isdigit(input[0])) return input[0] - '0';

  for (auto const& [key, val] : digitMap)
    if (input.substr(0, key.size()) == key) return val;

  return -1;
}

int part_2(string input) {
  string line;
  auto sum = 0;
  stringstream ss(input);
  int count = 0;

  while (getline(ss, line, '\n')) {
    count++;
    vector<int> digits;

    for (int i = 0; i < line.size(); ++i) {
      int con = convert(line.substr(i, line.size()));
      if (con != -1) digits.push_back(con);
    }

    if (digits.size() == 0) continue;
    sum += 10 * digits.front() + digits.back();
  }

  return sum;
}

int main() {
  ifstream t("../input.txt");
  stringstream input;
  input << t.rdbuf();

  cout << "Part 1: " << part_1(input.str()) << endl;
  cout << "Part 2: " << part_2(input.str()) << endl;

  return 0;
}

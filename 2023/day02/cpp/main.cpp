#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

#define RED_MAX 12
#define GREEN_MAX 13
#define BLUE_MAX 14

string read_to_string(string const& file_name) {
  ifstream file_stream(file_name);
  stringstream buffer;
  buffer << file_stream.rdbuf();
  return string(buffer.str());
}

vector<string> read_to_lines(string file_name) {
  ifstream file_stream(file_name);
  stringstream buffer;
  buffer << file_stream.rdbuf();

  vector<string> lines;
  string line;
  while (getline(buffer, line)) lines.push_back(line);
  return lines;
}

int main() {
  auto lines = read_to_lines("../input.txt");

  int part1 = 0;
  int part2 = 0;
  int line_number = 0;
  for (auto line : lines) {
    line_number++;
    line = line.substr(line.find(":") + 1, line.size() - line.find(":") - 1);

    bool line_possible = true;
    size_t curr_word_start = 0;

    int red_max = 0;
    int green_max = 0;
    int blue_max = 0;

    for (size_t i = 0; i < line.size(); ++i) {
      char c = line[i];

      if (c == ',' || c == ';' || i == line.size() - 1) {
        string word = line.substr(curr_word_start, i - curr_word_start + 1);
        int value = stoi(word);

        if (word.find("red") != string::npos) {
          red_max = max(red_max, value);
          if (value > RED_MAX) line_possible = false;
        } else if (word.find("green") != string::npos) {
          green_max = max(green_max, value);
          if (value > GREEN_MAX) line_possible = false;
        } else if (word.find("blue") != string::npos) {
          blue_max = max(blue_max, value);
          if (value > BLUE_MAX) line_possible = false;
        }
        curr_word_start = i + 1;
      }
    }
    part2 += (max(1, red_max) * max(1, green_max) * max(1, blue_max));
    if (line_possible) part1 += line_number;
  }

  cout << "Part 1: " << part1 << endl;
  cout << "Part 2: " << part2 << endl;

  return 0;
}

#include <iostream>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <variant>

using namespace std;

enum TokenType {
  INT,
  FLOAT
};

struct Token {
  TokenType token_type;
  variant<int, float> value;
};

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
  string content = read_to_string("main.cpp");
  cout << content << endl;
  return 0;
}

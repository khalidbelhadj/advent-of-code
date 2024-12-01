#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

std::string read_to_string(std::string const& file_name) {
  std::ifstream file_stream(file_name);
  std::stringstream buffer;
  buffer << file_stream.rdbuf();
  return std::string(buffer.str());
}

std::vector<std::string> read_to_lines(std::string file_name) {
  std::ifstream file_stream(file_name);
  std::stringstream buffer;
  buffer << file_stream.rdbuf();

  std::vector<std::string> lines;
  std::string line;
  while (getline(buffer, line)) lines.push_back(line);
  return lines;
}
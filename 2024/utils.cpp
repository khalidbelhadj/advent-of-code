#include <climits>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>

std::string read_to_string(std::string const& file_name) {
  std::ifstream file_stream(file_name);
  if (file_stream.fail() | file_stream.bad()) {
    std::cerr << "Failed to open file: " << file_name << std::endl;
    exit(1);
  }
  std::stringstream buffer;
  buffer << file_stream.rdbuf();
  return std::string(buffer.str());
}

std::vector<std::string> read_to_lines(const std::string& file_name) {
  std::ifstream file_stream(file_name);
  if (file_stream.fail() | file_stream.bad()) {
    std::cerr << "Failed to open file: " << file_name << std::endl;
    exit(1);
  }
  std::stringstream buffer;
  buffer << file_stream.rdbuf();

  std::vector<std::string> lines;
  std::string line;
  while (getline(buffer, line)) lines.push_back(line);
  return lines;
}

template <typename K, typename V>
const V& get_or(const std::unordered_map<K, V>& m, const K& key,
                const V& defval) {
  auto it = m.find(key);
  if (it == m.end()) {
    return defval;
  } else {
    return it->second;
  }
}

template <typename K, typename V>
std::optional<V> get_option(const std::unordered_map<K, V>& m, const K& key) {
  auto it = m.find(key);
  if (it == m.end()) {
    return std::nullopt;
  } else {
    return it->second;
  }
}
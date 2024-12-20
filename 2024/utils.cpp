#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

std::string read_to_string(std::string const &file_name) {
    std::ifstream file_stream(file_name);
    if (file_stream.fail() || file_stream.bad()) {
        std::cerr << "Failed to open file: " << file_name << std::endl;
        exit(1);
    }
    std::stringstream buffer;
    buffer << file_stream.rdbuf();
    return std::string(buffer.str());
}

std::vector<std::string> read_to_lines(const std::string &file_name) {
    std::ifstream file_stream(file_name);
    if (file_stream.fail() || file_stream.bad()) {
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
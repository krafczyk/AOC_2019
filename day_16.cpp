#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdint>
#include <cmath>

int mask[4] = {0, 1, 0, -1};

std::vector<uint8_t> toMessageData(const std::string& input) {
    std::vector<uint8_t> result;
    for (char c : input) {
        result.push_back(c-48);
    }
    return result;
}

std::string toString(const std::vector<uint8_t>& message_data) {
    std::string result;
    for (uint8_t v : message_data) {
        result.push_back((char)(v+48));
    }
    return result;
}

void applyFunc(std::vector<uint8_t>& res, const std::vector<uint8_t>& data) {
    int64_t len = (int64_t) data.size();
    res.clear();
    for (int64_t I=1; I <= len; ++I) {
        int64_t sum = 0;
        for(int64_t i=0; i < len; ++i) {
            sum += mask[((i+1)/I)%4]*data[i];
        }
        res.push_back(std::abs(sum)%10);
    }
}

void iterateApp(std::vector<uint8_t>& res, std::vector<uint8_t>& in) {
    // apply function
    applyFunc(res, in);
    // Swap buffers
    std::swap(in, res);
}

void iterateAppRepeat(std::vector<uint8_t>& res, std::vector<uint8_t>& in, size_t N, bool report=false) {
    for (size_t i = 0; i < N; ++i) {
        iterateApp(res, in);
        if(report) {
            //std::cout << (i+1) << "\r" << std::flush;
            std::cout << (i+1) << std::endl;
        }
    }
}

void task1(const std::vector<uint8_t>& in_msg) {
    std::vector<uint8_t> msg = in_msg;
    std::vector<uint8_t> msg2 = in_msg;
    iterateAppRepeat(msg2, msg, 100);
    std::string result = toString(msg);
    std::cout << "Task 1: " << result.substr(0,8) << std::endl;
}

void task2(const std::vector<uint8_t>& in_msg) {
    // Get offset
    size_t offset = 0;
    for (int i = 0; i < 7; i++) {
        offset += ((size_t)in_msg[6-i])*((size_t)pow(10,i));
    }
    std::cout << "offset: " << offset << std::endl;
    // Expand message 10000 times.
    std::vector<uint8_t> full_msg;
    full_msg.reserve(10000*in_msg.size());
    for(int i=0; i < 10000; ++i) {
        full_msg.insert(full_msg.end(), in_msg.cbegin(), in_msg.cend());
    }
    std::vector<uint8_t> full_msg2;
    full_msg2.reserve(full_msg.size());
    iterateAppRepeat(full_msg2, full_msg, 100, true);
    std::string result = toString(full_msg);
    std::cout << "Task 2: " << result.substr(offset,8) << std::endl;
}

int main(int argc, char** argv) {
    std::string data_filepath = "data/day_16.txt";
    if (argc > 1) {
        data_filepath = std::string(argv[1]);
    }
    std::ifstream infile;
    infile.open(data_filepath);
    std::string line;
    if(!std::getline(infile, line)) {
        std::cerr << "There was a problem reading the input file." << std::endl;
        return 1;
    }
    std::vector<uint8_t> msg_data = toMessageData(line);
    task1(msg_data);
    task2(msg_data);
    return 0;
}

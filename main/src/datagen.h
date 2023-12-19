#include <string>
#include <vector>
#include <cstdint>
#include <bitset>
#include "search.h"
#include "board.h"

#ifndef EUREKA_DATAGEN_H
#define EUREKA_DATAGEN_H

typedef std::pair<std::bitset<781>, int32_t> entry;

class datagenThread {
public:
    datagenThread() = default;
    void start(const std::vector<std::string> &fens, int32_t totalPositions_);
    std::vector<entry> entries;
private:
    int32_t totalPositions = 0, positionsLogged = 0;
    void runGame();
    void processFEN(int32_t depth = 4);
    board b;
    engine e;
};

class datagen {
public:
    datagen();
private:
    void logEntry(entry e, std::ofstream &fout);
};

#endif
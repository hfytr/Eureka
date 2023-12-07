#include <string>
#include <chrono>
#include <iostream>
#include "board.h"

using namespace std::chrono;

#ifndef PERFT_H
#define PERFT_H
const uint64_t perftResult[5] = {4865609ULL,4085603ULL,11030083ULL,15833292ULL,89941194ULL};
const int32_t perftDepth[5] = {5,4,6,5,5};
const std::string fen[5] = {
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
};

class perft {
public:
    perft();

private:
    board b;
    int32_t search(int32_t depth);
};
#endif

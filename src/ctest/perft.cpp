#include <string>
#include <chrono>
#include <iostream>
#include <cstdint>
#include "../board.h"
#include "perft.h"

using namespace std::chrono;

// used to test move generation, loops through a series of positions, finding number of nodes at given depth
// duplicated ARE counted
int32_t perft::runPerft(int32_t i){
    time_point<steady_clock> start = steady_clock::now();
    int32_t total = 0;
    b = Board(fen[i]);
    Zobrist startZobrist = b.getZobrist();
    int32_t result = search(perftDepth[i]);
    total += result;
    std::cout << "Number of nodes: " << result << " Expected number of nodes: " << perftResult[i]
              << "\nStart Zobrist: " << startZobrist.getKey() << " End Zobrist: " << b.getZobrist().getKey()
              << "\nPosition:\n" << b.print() << std::endl;
    if (result != perftResult[i] || b.getZobrist() != startZobrist)
        return 1;

    auto passed = steady_clock::now()-start;
    std::cout << "Nodes/second: " << total/(duration_cast<milliseconds>(passed).count()+1)*1000;
    return 0;
}

uint32_t perft::search(uint8_t depth){
    uint32_t numMoves = 0;
    moveList moves = b.genMoves(true,false);
    if (depth == 1)
        return moves.len();
    int32_t result;
    for (int32_t i = 0; i < moves.len(); i++){
        b.makeMove(moves[i]);
        result = search(depth-1);
        numMoves += result;
        b.unmakeMove();
        if (depth == 6)
            std::cout << moves[i].print() << ' ' << result << '\n';
    }
    return numMoves;
}
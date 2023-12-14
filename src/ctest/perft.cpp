#include <string>
#include <chrono>
#include <iostream>
#include "../board.h"
#include "perft.h"

using namespace std::chrono;

// used to test move generation, loops through a series of positions, finding number of nodes at given depth
// duplicated ARE counted
int32_t perft::runPerft(int32_t i){
    time_point<steady_clock> start = steady_clock::now();
    int32_t total = 0;
        b = board(fen[i]);
        uint64_t startZobrist = b.zobrist;
        int32_t result = search(perftDepth[i]);
        total += result;
        std::cout << "FAILED\nNumber of nodes: " << result << " Expected number of nodes: " << perftResult[i]
                  << "\nStart Zobrist: " << startZobrist << " End Zobrist: " << b.zobrist
                  << "\nPosition:\n" << b.toString() << std::endl;
        if (result != perftResult[i] || b.zobrist != startZobrist)
            return b.zobrist != startZobrist * 2 + result != perftResult[i];

    auto passed = steady_clock::now()-start;
    std::cout << "Nodes/second: " << total/(duration_cast<milliseconds>(passed).count()/1000);
    return 0;
}

int32_t perft::search(int32_t depth){
    int32_t numMoves = 0;
    moveList moves = b.genMoves(true,false);
    if (depth == 1)
        return moves.len();
    int32_t result;
    for (int32_t i = 0; i < moves.len(); i++){
        b.makeMove(moves[i]);
        result = search(depth-1);
        numMoves += result;
        b.unmakeMove();
    }
    return numMoves;
}
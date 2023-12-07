#include <string>
#include <chrono>
#include <iostream>
#include "board.h"
#include "perft.h"

using namespace std::chrono;

// used to test move generation, loops through a series of positions, finding number of nodes at given depth
// duplicated ARE counted
perft::perft(){
    time_point<steady_clock> start = steady_clock::now();
    int32_t total = 0;
    for (int32_t i = 0; i < 5; i++){
        b = board(fen[i]);
        int32_t result = search(perftDepth[i]);
        total += result;
        std::cout << "Number of nodes: " << result << "\nExpected number of nodes: " << perftResult[i] << "\nPosition:\n" << b.toString();
    }
    auto passed = steady_clock::now()-start;
    std::cout << "Nodes/second: " << total/(duration_cast<chrono::milliseconds>(passed).count()/1000);
}

int32_t perft::search(int32_t depth){
    int32_t numMoves = 0;
    moveList moves = b.genMoves(true,false);
    if (depth == 1)
        return moves.len();
    for (int32_t i = 0; i < moves.len(); i++){
        b.makeMove(moves[i]);
        numMoves += search(depth-1);
        b.unmakeMove();
    }
    return numMoves;
}
#include <string>
#include <chrono>
#include <iostream>
#include "board.h"
#include "perft.h"
using namespace std;
using namespace std::chrono;

// used to test move generation, loops through a series of positions, finding number of nodes at given depth
// duplicated ARE counted
perft::perft(){
    time_point<steady_clock> start = steady_clock::now();
    int32_t total = 0;
    for (int32_t i = 2; i < 5; i++){
        b = board(fen[i]);
        cout << b.phase << ' ' << perftDepth[i] << endl;
        int32_t result = search(perftDepth[i]);
        total += result;
        cout << "Number of nodes: " << result << "\nExpected number of nodes: " << perftResult[i] << "\nPosition:\n" << b.toString();
        cout << b.phase << endl;
    }
    auto passed = steady_clock::now()-start;
    cout << "Nodes/second: " << total/(duration_cast<chrono::milliseconds>(passed).count()/1000);
}

int32_t perft::search(int32_t depth){
    int32_t numMoves = 0, result = 0;
    moveList moves = b.genMoves(true,false);
    if (depth == 1)
        return moves.len;
    for (int32_t i = 0; i < moves.len; i++){
        b.makeMove(moves[i]);
        result = search(depth-1);
        numMoves += result;
        b.unmakeMove();
    }
    return numMoves;
}
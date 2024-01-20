#ifndef SEARCH_H
#define SEARCH_H
#include <vector>
#include <string>
#include <chrono>
#include <climits>
#include <cstdint>
#include "tt.h"
#include "board.h"
#include "constants.h"

using namespace std::chrono;


enum TaskMode {
    OPEN=0,
    TIME=1,
    NODES=2,
    DEPTH=3
};

class task{
public:
    uint16_t increment[2], timeLeft[2], movestogo, length;
    TaskMode mode;
    moveList moves;
    bool ponder;
    task(uint16_t winc_ = 0, uint16_t binc_ = 0, uint16_t wtime_ = -1, uint16_t btime_ = -1, uint16_t movestogo_ = -1, TaskMode mode_ = OPEN, int16_t length_ = 2000, bool ponder_ = false){
        increment[0]=winc_;increment[1]=binc_;timeLeft[0]=wtime_;timeLeft[1]=btime_;movestogo=movestogo_;mode=mode_;length=length_;ponder=ponder_;
    }
};

class engine {
public:
    Board b;
    uint64_t nodes;
    task t;
    std::vector<std::vector<Move>> pv;
    std::vector<Move> lastpv;
    std::vector<std::vector<Move>> killers;
    time_point<steady_clock> start;
    uint16_t fullDepth = 1;
    int32_t moveOverhead = 20;
    TT tt = TT(DEFAULTTTSIZE);
    int32_t butterfly[2][64][64] = {};
    bool over{}, forceStop = false;
    uint8_t rootType = PV_NODE, debug = 0;

    int32_t see(Move m = 0, uint8_t sq = 64);
    int32_t negamax(uint8_t depth, int32_t alpha, int32_t beta), quiesce(int32_t alpha, int32_t beta);
    uint32_t initialTime();
    xMove search(uint8_t depth, int32_t alpha, int32_t beta);
    xMove getMove(task t);
    bool checkOver(), isDbgLine();
    void printInfo(int32_t eval);
    engine()= default;

    bool isNullWindow(int64_t alpha, int64_t beta){
        return beta - alpha == 1;
    }
};

class scoredMoveList : public moveList {
public:
    scoredMoveList(uint8_t depth_, std::vector<Move> killers_, engine* e_, moveList list);
    Move get();
private:
    int32_t scores[256], see[256], i = 0;
    uint8_t depth;
    bool ispv;
    std::vector<Move> killers;
    engine* e;
    int32_t scoreMove();
};

#endif

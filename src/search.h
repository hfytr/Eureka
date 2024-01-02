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

#define FUTILITY_MARGIN 250

enum TaskMode {
    OPEN=0,
    TIME=1,
    NODES=2,
    DEPTH=3
};

class task{
public:
    uint32_t increment[2], timeLeft[2], movestogo = MAX32, length;
    TaskMode mode;
    moveList moves;
    bool ponder;
    task(uint32_t winc_ = 0, uint32_t binc_ = 0, uint32_t wtime_ = 0, uint32_t btime_ = 0, uint32_t movestogo_ = MAX32, TaskMode mode_ = OPEN, uint32_t length_ = 0, bool ponder_ = false){
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
    int32_t eval, moveOverhead = 20;
    TT tt = TT(DEFAULTTTSIZE);
    int32_t butterfly[2][64][64] = {};
    bool over{}, forceStop = false;
    NodeType rootType = PV_NODE;
    int8_t debug = 0;

    int32_t see(Move m = 0, uint8_t sq = 64);
    int32_t negamax(uint8_t depth, int32_t alpha, int32_t beta), quiesce(int32_t alpha, int32_t beta);
    uint32_t initialTime();
    xMove search(uint8_t depth, int32_t alpha, int32_t beta);
    xMove getMove(task t);
    bool checkOver(), isDbgLine();
    void printInfo(xMove best);
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
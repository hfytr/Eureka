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
using namespace std;
using namespace std::chrono;

class xMove{
public:
    int32_t eval;
    uint16_t m;
    xMove(int32_t eval_=0, uint16_t m_=0){
        eval=eval_;m=m_;
    }
};

class task{
public:
    int32_t increment[2], timeLeft[2], movestogo, length, mode;
    moveList moves;
    bool ponder;
    task(int32_t winc_ = 0, int32_t binc_ = 0, int32_t wtime_ = -1, int32_t btime_ = -1, int32_t movestogo_ = -1, int32_t mode_ = -1, int32_t length_ = 2000, bool ponder_ = false){
        increment[0]=winc_;increment[1]=binc_;timeLeft[0]=wtime_;timeLeft[1]=btime_;movestogo=movestogo_;mode=mode_;length=length_;ponder=ponder_;
    }
};

class engine {
public:
    board b;
    int64_t nodes;
    task t;
    vector<vector<uint16_t>> pv;
    vector<int32_t> pveval;
    time_point<steady_clock> start;
    int32_t milli = 2 * 1000, fullDepth = 0, selDepth = 0;
    TT tt = TT(TTSIZE);
    int32_t butterfly[2][64][64] = {}, count = 0;
    bool over, debug = false, forceStop;

    int32_t scoreMove(uint16_t m, int32_t depth, pair<uint16_t,uint16_t> killer, bool ispv);
    int32_t negamax(int32_t depth, int32_t alpha, int32_t beta, pair<uint16_t,uint16_t> killerOpp, pair<uint16_t,uint16_t> &killer, vector<uint16_t> &parentpv, bool ispv);
    int32_t initialTime();
    uint16_t search(int32_t depth);
    uint16_t getMove(task t);
    bool checkOver();
    engine(){};
};
#endif
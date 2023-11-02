#ifndef ENGINE_H
#define ENGINE_H

#include <vector>
#include <string>
#include <chrono>
#include <climits>
#include "board.h"
#include "constants.h"
using namespace std;
using namespace std::chrono;

typedef pair<int32_t,uint16_t> xMove;

class TTnode {
public:
    uint64_t hash;
    int32_t eval;
    uint16_t m;
    uint8_t age, depth;

    TTnode(uint64_t hash_=0, int32_t eval_=0, uint16_t m_=0, uint8_t age_=0, uint8_t depth_=0){
        hash=hash_;eval=eval_;m=m_;age=age_;depth=depth_;
    }
};

class TT {
public:
    int32_t size, trueLen=0;
    TTnode* container[2];
    TT(int32_t s);
    void push(TTnode a), clear();
    TTnode get(uint64_t hash);
};

class task{
public:
    int32_t increment[2], timeLeft[2], movestogo, length, mode;
    moveList moves;
    bool ponder;
    task(int32_t winc_ = 0, int32_t binc_ = 0, int32_t wtime_ = -1, int32_t btime_ = -1, int32_t movestogo_ = -1, int32_t mode_ = 0, int32_t length_ = 2000, bool ponder_ = false){
        increment[0]=winc_;increment[1]=binc_;timeLeft[0]-wtime_;timeLeft[1]=btime_;movestogo=movestogo_;mode=mode_;length=length_;ponder=ponder_;
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
    int32_t milli = 2 * 1000, fullDepth = 0;
    TT tt = TT(TTSIZE);
    int32_t butterfly[2][64][64] = {}, count = 0;
    bool over, debug;

    int32_t scoreMove(uint16_t m, int32_t depth, pair<uint16_t,uint16_t> killer, bool ispv);
    int32_t negamax(int32_t depth, int32_t alpha, int32_t beta, pair<uint16_t,uint16_t> killerOpp, pair<uint16_t,uint16_t> &killer, vector<uint16_t> &parentpv, bool ispv);
    uint16_t search(int32_t depth);
    uint16_t getMove(task t);
    bool checkOver();
    engine(){};
};
#endif
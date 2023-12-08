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
    int64_t nodes{};
    task t;
    std::vector<std::vector<uint16_t>> pv;
    std::vector<int32_t> pveval;
    std::vector<std::pair<uint16_t,uint16_t>> killers;
    time_point<steady_clock> start;
    int32_t fullDepth = 0;
    TT tt = TT(DEFAULTTTSIZE);
    int32_t butterfly[2][64][64] = {};
    bool over{}, debug = false, forceStop = false;
    uint8_t rootType = PV_NODE;

    int32_t see(uint16_t m = 0, int32_t sq = -1);
    int32_t negamax(uint8_t depth, int32_t alpha, int32_t beta, std::vector<uint16_t> &parentpv, bool ispv), quiesce(int32_t alpha, int32_t beta);
    int32_t initialTime();
    xMove search(uint8_t depth, int32_t alpha, int32_t beta);
    uint16_t getMove(task t);
    bool checkOver(), isDbgLine();
    void printInfo();
    engine()= default;

    inline bool isNullWindow(int64_t alpha, int64_t beta){
        return std::abs(alpha-beta) == 1;
    }
};

class scoredMoveList : moveList {
public:
    scoredMoveList(uint8_t depth_, bool ispv_, std::pair<uint16_t,uint16_t> killers_, engine* e_, moveList list);
    uint16_t get();
    uint16_t len(){ return length; }
private:
    int32_t scores[256], i = 0;
    uint8_t depth;
    bool ispv;
    std::pair<uint16_t, uint16_t> killers;
    engine* e;
    int32_t scoreMove(uint16_t m);
};

#endif
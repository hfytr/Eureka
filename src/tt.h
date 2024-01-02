#ifndef TT_H
#define TT_H
#include <climits>
#include <cstdint>
#include <vector>
#include "constants.h"
#include "board.h"

class xMove : public Move {
public:
    int32_t eval;
    xMove(int32_t eval_ = 0, Move m = 0){
        eval=eval_;
        container = m.raw();
    }
    Move m() { return Move(container); }
};

enum NodeType {
    PV_NODE = 0,
    FAIL_HIGH = 1,
    FAIL_LOW = 2
};

class TTnode {
public:
    Zobrist hash;
    xMove m;
    NodeType type;
    uint8_t depth;

    TTnode(Zobrist hash_=0ULL, xMove m_=0ULL, uint8_t depth_=0, NodeType type_=PV_NODE){
        hash=hash_;m=m_;depth=depth_;type=type_;
    }
};

class TTbucket {
private:
    TTnode buckets[NUM_BUCKETS];
public:
    TTnode operator[](int32_t i);
};

class TT {
public:
    int32_t size;
    std::vector<TTbucket> container;
    TT(uint32_t s);
    void push(TTnode a), clear(), resize(uint32_t s);
    TTnode get(Zobrist hash);
};
#endif
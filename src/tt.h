#ifndef TT_H
#define TT_H
#include <climits>
#include <cstdint>

class TTnode {
public:
    uint64_t hash;
    int32_t eval;
    uint16_t m;
    uint8_t age, type;
    int8_t depth;

    TTnode(uint64_t hash_=0, int32_t eval_=0, uint16_t m_=0, uint8_t age_=0, uint8_t depth_=0, uint8_t type_ = 0){
        hash=hash_;eval=eval_;m=m_;age=age_;depth=depth_;type=type_;
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
#endif
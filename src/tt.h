#ifndef TT_H
#define TT_H
#include <climits>
#include <cstdint>
#include <vector>
#include "constants.h"


class TTnode {
public:
    uint64_t hash;
    int32_t eval;
    uint16_t m;
    uint8_t type;
    uint8_t depth;

    TTnode(uint64_t hash_=0, int32_t eval_=0, uint16_t m_=0, uint8_t depth_=0, uint8_t type_ = 0){
        hash=hash_;eval=eval_;m=m_;depth=depth_;type=type_;
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
    TTnode get(uint64_t hash);
};
#endif
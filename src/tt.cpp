#include "tt.h"
#include "constants.h"
#include <cstdint>

TTnode TTbucket::operator[](int32_t i){
    return buckets[i];
}

TT::TT(uint32_t s){
    size = s*1000000/(sizeof(TTnode)*NUM_BUCKETS);
    container.resize(size);
    clear();
}

void TT::resize(uint32_t s){
    size = s*1000000/(sizeof(TTnode)*NUM_BUCKETS);
    container.resize(size);
}

void TT::push(TTnode a){
    int32_t minDepth;
    for (int32_t i = 0; i < NUM_BUCKETS; i++){
        if (container[a.hash%size][i].hash == 0){
            container[a.hash%size][i] = a;
            return;
        } else if (i == 0 || container[a.hash%size][i].depth < container[a.hash%size][minDepth].depth)
            minDepth = i;
    }
    container[a.hash%size][minDepth] = a;
}

TTnode TT::get(uint64_t hash){
    for (int32_t i = 0; i < NUM_BUCKETS; i++)
        if (container[hash%size][i].hash == hash)
            return container[hash%size][i];
    return {};
}

void TT::clear(){
    for (int32_t i = 0; i < size; i++)
        for (int32_t j = 0; j < NUM_BUCKETS; j++)
            container[j][i] = TTnode();
}
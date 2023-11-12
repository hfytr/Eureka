#include "tt.h"
#include "constants.h"
#include <cstdint>

TT::TT(int32_t s){
    size = s;
    container[0] = new TTnode[size];
    container[1] = new TTnode[size];
}

// container[0] contains the newest entry's, while container[1] contains entry's with higher depth
// higher depth entry's will be deleted if they are too old
void TT::push(TTnode a){
    int32_t i = (a.depth <= container[0][a.hash%size].depth) || (a.age - container[0][a.hash%size].age <= AGETOLERANCE);
    container[i][a.hash%size] = a;
}

TTnode TT::get(uint64_t hash){
    int32_t i = container[0][hash%size].hash == hash ? 0 : (container[1][hash%size].hash == hash ? 1 : -1);
    if (i == -1)
        return TTnode();
    return container[i][hash%size];
}

void TT::clear(){
    for (int32_t i = 0; i < size; i++){
        container[0][i] = TTnode();
        container[1][i] = TTnode();
    }
    trueLen = 0;
}

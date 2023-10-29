#include <string>
#include "engine.h"
using namespace std;

#ifndef UI_H
#define UI_H
class ui {
public:
    ui();

private:
    engine e;
    void setPos(int32_t x, int32_t y);
    uint16_t shortFromAlgebraic(string a);
    string message(), algebraicFromShort(uint16_t m), printpvs();
};
#endif